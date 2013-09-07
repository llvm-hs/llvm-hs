{-# LANGUAGE FlexibleInstances #-}
import Control.Exception (SomeException, try)
import Control.Monad
import Data.Maybe
import Data.List (isPrefixOf, (\\), intercalate, stripPrefix)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Monoid
import Data.Char
import Distribution.Simple
import Distribution.Simple.Program
import Distribution.Simple.Setup hiding (Flag)
import Distribution.Simple.LocalBuildInfo
import Distribution.PackageDescription
import Distribution.Version
import System.Environment
import System.SetEnv
import Distribution.System

-- define these selectively in C files (where _not_ using HsFFI.h),
-- rather than universally in the ccOptions, because HsFFI.h currently defines them
-- without checking they're already defined and so causes warnings.
uncheckedHsFFIDefines = ["__STDC_LIMIT_MACROS"]

llvmVersion = Version [3,4] []

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

main = do
  let (ldLibraryPathVar, ldLibraryPathSep) = 
        case buildOS of
          OSX -> ("DYLD_LIBRARY_PATH",":")
          _ -> ("LD_LIBRARY_PATH",":")
      addToLdLibraryPath s = do
         v <- try $ getEnv ldLibraryPathVar :: IO (Either SomeException String)
         setEnv ldLibraryPathVar (s ++ either (const "") (ldLibraryPathSep ++) v)
      getLLVMConfig configFlags = do
         let verbosity = fromFlag $ configVerbosity configFlags
         (program, _, _) <- requireProgramVersion verbosity llvmProgram
                            (withinVersion llvmVersion)
                            (configPrograms configFlags)
         let llvmConfig :: [String] -> IO String
             llvmConfig = getProgramOutput verbosity program
         return llvmConfig
      addLLVMToLdLibraryPath configFlags = do
        llvmConfig <- getLLVMConfig configFlags
        [libDir] <- liftM lines $ llvmConfig ["--libdir"]
        addToLdLibraryPath libDir
         
  defaultMainWithHooks simpleUserHooks {
    hookedPrograms = [ llvmProgram ],

    confHook = \(genericPackageDescription, hookedBuildInfo) configFlags -> do
      llvmConfig <- getLLVMConfig configFlags

      llvmCppFlags <- do
        l <- llvmConfig ["--cppflags"]
        return $ (filter ("-D" `isPrefixOf`) $ words l) \\ (map ("-D"++) uncheckedHsFFIDefines)
      includeDirs <- liftM lines $ llvmConfig ["--includedir"]
      libDirs@[libDir] <- liftM lines $ llvmConfig ["--libdir"]
      [llvmVersion] <- liftM lines $ llvmConfig ["--version"]
      let sharedLib = case llvmVersion of
                        "3.2" -> "LLVM-3.2svn"
                        x -> "LLVM-" ++ x
      staticLibs <- liftM (map (fromJust . stripPrefix "-l") . words) $ llvmConfig ["--libs"]
      externLibs <- liftM (mapMaybe (stripPrefix "-l") . words) $ llvmConfig ["--ldflags"]

      let genericPackageDescription' = genericPackageDescription {
            condLibrary = do
              libraryCondTree <- condLibrary genericPackageDescription
              return libraryCondTree {
                condTreeData = condTreeData libraryCondTree <> mempty {
                    libBuildInfo = mempty { ccOptions = llvmCppFlags }
                  },
                condTreeComponents = condTreeComponents libraryCondTree ++ [
                  (
                    Var (Flag (FlagName "shared-llvm")),
                    CondNode (mempty { libBuildInfo = mempty { extraLibs = [sharedLib] ++ externLibs } }) [] [],
                    Just (CondNode (mempty { libBuildInfo = mempty { extraLibs = staticLibs ++ externLibs } }) [] [])
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

    buildHook = \packageDescription localBuildInfo userHooks buildFlags -> do
      addLLVMToLdLibraryPath (configFlags localBuildInfo)
      buildHook simpleUserHooks packageDescription localBuildInfo userHooks buildFlags,

    testHook = \packageDescription localBuildInfo userHooks testFlags -> do
      addLLVMToLdLibraryPath (configFlags localBuildInfo)
      testHook simpleUserHooks packageDescription localBuildInfo userHooks testFlags,

    haddockHook = \packageDescription localBuildInfo userHooks haddockFlags -> do
       let v = "GHCRTS"
       oldGhcRts <- try $ getEnv v :: IO (Either SomeException String)
       setEnv v (either (const id) (\o n -> o ++ " " ++ n) oldGhcRts "-K32M")
       haddockHook simpleUserHooks packageDescription localBuildInfo userHooks haddockFlags
       either (const (unsetEnv v)) (setEnv v) oldGhcRts
   }
