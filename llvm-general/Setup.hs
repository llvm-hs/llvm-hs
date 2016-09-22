{-# LANGUAGE FlexibleInstances #-}
import Control.Exception (SomeException, try)
import Control.Monad
import Data.Functor
import Data.Maybe
import Data.List (isPrefixOf, (\\), intercalate, stripPrefix, find)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Monoid
import Data.Char
import Distribution.Simple
import Distribution.Simple.PreProcess
import Distribution.Simple.Program
import Distribution.Simple.Setup hiding (Flag)
import Distribution.Simple.LocalBuildInfo
import Distribution.PackageDescription
import Distribution.Version
import System.Environment
import Distribution.System

-- define these selectively in C files (where _not_ using HsFFI.h),
-- rather than universally in the ccOptions, because HsFFI.h currently defines them
-- without checking they're already defined and so causes warnings.
uncheckedHsFFIDefines = ["__STDC_LIMIT_MACROS"]

llvmVersion = Version [3,9] []

llvmConfigNames = [
  "llvm-config-" ++ (intercalate "." . map show . versionBranch $ llvmVersion),
  "llvm-config"
 ]

findJustBy :: Monad m => (a -> m (Maybe b)) -> [a] -> m (Maybe b)
findJustBy f (x:xs) = do
  x' <- f x
  case x' of
    Nothing -> findJustBy f xs
    j -> return j
findJustBy _ [] = return Nothing

class ProgramSearch a where
  programSearch :: (String -> a) -> a

-- this instance is used before Cabal-1.18.0, when programFindLocation took one argument
instance Monad m => ProgramSearch (v -> m (Maybe b)) where
  programSearch checkName = \v -> findJustBy (\n -> checkName n v) llvmConfigNames

-- this instance is used for and after Cabal-1.18.0, when programFindLocation took two arguments
instance Monad m => ProgramSearch (v -> p -> m (Maybe b)) where
  programSearch checkName = \v p -> findJustBy (\n -> checkName n v p) llvmConfigNames

class OldHookable hook where
  preHookOld :: (PackageDescription -> LocalBuildInfo -> UserHooks -> TestFlags -> IO ()) -> hook -> hook

-- this instance is used before Cabal-1.22.0.0, when testHook took four arguments
instance OldHookable (PackageDescription -> LocalBuildInfo -> UserHooks -> TestFlags -> IO ()) where
  preHookOld f hook = \packageDescription localBuildInfo userHooks testFlags -> do
    f packageDescription localBuildInfo userHooks testFlags
    hook packageDescription localBuildInfo userHooks testFlags

-- this instance is used for and after Cabal-1.22.0.0, when testHook took four five arguments
instance OldHookable (Args -> PackageDescription -> LocalBuildInfo -> UserHooks -> TestFlags -> IO ()) where
  preHookOld f hook = \args packageDescription localBuildInfo userHooks testFlags -> do
    f packageDescription localBuildInfo userHooks testFlags
    hook args packageDescription localBuildInfo userHooks testFlags

llvmProgram :: Program
llvmProgram = (simpleProgram "llvm-config") {
  programFindLocation = programSearch (programFindLocation . simpleProgram),
  programFindVersion = 
    let
      stripSuffix suf str = let r = reverse in liftM r (stripPrefix (r suf) (r str))
      svnToTag v = maybe v (++"-svn") (stripSuffix "svn" v)
      trim = dropWhile isSpace . reverse . dropWhile isSpace . reverse
    in
      \v p -> findProgramVersion "--version" (svnToTag . trim) v p
 }

getLLVMConfig :: ConfigFlags -> IO (String -> IO String)
getLLVMConfig configFlags = do
  let verbosity = fromFlag $ configVerbosity configFlags
  (program, _, _) <- requireProgramVersion verbosity llvmProgram
                     (withinVersion llvmVersion)
                     (configPrograms configFlags)
  return $ getProgramOutput verbosity program . return

addToLdLibraryPath :: String -> IO ()
addToLdLibraryPath path = do
  let (ldLibraryPathVar, ldLibraryPathSep) =
        case buildOS of
          OSX -> ("DYLD_LIBRARY_PATH",":")
          _ -> ("LD_LIBRARY_PATH",":")
  v <- try $ getEnv ldLibraryPathVar :: IO (Either SomeException String)
  setEnv ldLibraryPathVar (path ++ either (const "") (ldLibraryPathSep ++) v)

addLLVMToLdLibraryPath :: ConfigFlags -> IO ()
addLLVMToLdLibraryPath configFlags = do
  llvmConfig <- getLLVMConfig configFlags
  [libDir] <- liftM lines $ llvmConfig "--libdir"
  addToLdLibraryPath libDir

-- | These flags are not relevant for us and dropping them allows
-- linking against LLVM build with Clang using GCC
ignoredCxxFlags :: [String]
ignoredCxxFlags =
  ["-Wcovered-switch-default", "-fcolor-diagnostics"] ++ map ("-D" ++) uncheckedHsFFIDefines

ignoredCFlags :: [String]
ignoredCFlags = ["-Wcovered-switch-default", "-Wdelete-non-virtual-dtor", "-fcolor-diagnostics"]

main = do
  let origUserHooks = simpleUserHooks
                  
  defaultMainWithHooks origUserHooks {
    hookedPrograms = [ llvmProgram ],

    confHook = \(genericPackageDescription, hookedBuildInfo) configFlags -> do
      llvmConfig <- getLLVMConfig configFlags
      llvmCxxFlags <- do
        rawLlvmCxxFlags <- llvmConfig "--cxxflags"
        return (words rawLlvmCxxFlags \\ ignoredCxxFlags)
      let stdLib = maybe "stdc++"
                         (drop (length stdlibPrefix))
                         (find (isPrefixOf stdlibPrefix) llvmCxxFlags)
            where stdlibPrefix = "-stdlib=lib"
      includeDirs <- liftM lines $ llvmConfig "--includedir"
      libDirs@[libDir] <- liftM lines $ llvmConfig "--libdir"
      [llvmVersion] <- liftM lines $ llvmConfig "--version"
      let sharedLib = case llvmVersion of
                        "3.2" -> "LLVM-3.2svn"
                        x -> "LLVM-" ++ x
      staticLibs <- liftM (map (fromJust . stripPrefix "-l") . words) $ llvmConfig "--libs"
      systemLibs <- liftM (map (fromJust . stripPrefix "-l") . words) $ llvmConfig "--system-libs"

      let genericPackageDescription' = genericPackageDescription {
            condLibrary = do
              libraryCondTree <- condLibrary genericPackageDescription
              return libraryCondTree {
                condTreeData = condTreeData libraryCondTree <> mempty {
                    libBuildInfo =
                      mempty {
                        ccOptions = llvmCxxFlags,
                        extraLibs = [stdLib]
                      }
                  },
                condTreeComponents = condTreeComponents libraryCondTree ++ [
                  (
                    Var (Flag (FlagName "shared-llvm")),
                    CondNode (mempty { libBuildInfo = mempty { extraLibs = [sharedLib] ++ systemLibs } }) [] [],
                    Just (CondNode (mempty { libBuildInfo = mempty { extraLibs = staticLibs ++ systemLibs } }) [] [])
                  )
                ] 
              }
           }
          configFlags' = configFlags {
            configExtraLibDirs = libDirs ++ configExtraLibDirs configFlags,
            configExtraIncludeDirs = includeDirs ++ configExtraIncludeDirs configFlags
           }
      addLLVMToLdLibraryPath configFlags'
      confHook simpleUserHooks (genericPackageDescription', hookedBuildInfo) configFlags',

    hookedPreProcessors =
      let origHookedPreprocessors = hookedPreProcessors origUserHooks
          newHsc buildInfo localBuildInfo =
              PreProcessor {
                  platformIndependent = platformIndependent (origHsc buildInfo localBuildInfo),
                  runPreProcessor = \inFiles outFiles verbosity -> do
                      llvmConfig <- getLLVMConfig (configFlags localBuildInfo)
                      llvmCFlags <- do
                          rawLlvmCFlags <- llvmConfig "--cflags"
                          return (words rawLlvmCFlags \\ ignoredCFlags)
                      let buildInfo' = buildInfo { ccOptions = llvmCFlags }
                      runPreProcessor (origHsc buildInfo' localBuildInfo) inFiles outFiles verbosity
              }
              where origHsc = fromMaybe ppHsc2hs (lookup "hsc" origHookedPreprocessors)
      in [("hsc", newHsc)] ++ origHookedPreprocessors,

    buildHook = \packageDescription localBuildInfo userHooks buildFlags -> do
          addLLVMToLdLibraryPath (configFlags localBuildInfo)
          buildHook origUserHooks packageDescription localBuildInfo userHooks buildFlags,

    testHook = preHookOld (\_ localBuildInfo _ _ -> addLLVMToLdLibraryPath (configFlags localBuildInfo))
               (testHook origUserHooks)
   }
