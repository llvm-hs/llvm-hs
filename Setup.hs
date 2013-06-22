import Control.Monad
import Data.List (isPrefixOf, (\\), intercalate)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Monoid
import Distribution.Simple
import Distribution.Simple.Program
import Distribution.Simple.Setup
import Distribution.Simple.LocalBuildInfo
import Distribution.PackageDescription
import System.Environment
import System.SetEnv
import Distribution.System



-- define these selectively in C files (where _not_ using HsFFI.h),
-- rather than universally in the ccOptions, because HsFFI.h currently defines them
-- without checking they're already defined and so causes warnings.
uncheckedHsFFIDefines = ["__STDC_LIMIT_MACROS"]

llvmProgram = simpleProgram "llvm-config"

main = do
  let (ldLibraryPathVar, ldLibraryPathSep) = 
        case buildOS of
          OSX -> ("DYLD_LIBRARY_PATH",":")
          _ -> ("LD_LIBRARY_PATH",":")
      addToLdLibraryPath s = do
         v <- lookupEnv ldLibraryPathVar
         setEnv ldLibraryPathVar (s ++ maybe "" (ldLibraryPathSep ++) v)
      getLLVMConfig configFlags = do
         let verbosity = fromFlag $ configVerbosity configFlags
         -- preconfigure the configuration-generating program "llvm-config"
         programDb <- configureProgram verbosity llvmProgram
                      . userSpecifyPaths (configProgramPaths configFlags)
                      . userSpecifyArgss (configProgramArgs configFlags)
                      $ configPrograms configFlags
         let llvmConfig :: [String] -> IO String
             llvmConfig = getDbProgramOutput verbosity llvmProgram programDb
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
      let libraryVersion = 
              case llvmVersion of
                "3.2" -> "3.2svn"
                x -> x

      let genericPackageDescription' = genericPackageDescription {
            condLibrary = do
              libraryCondTree <- condLibrary genericPackageDescription
              return libraryCondTree {
                condTreeData = condTreeData libraryCondTree <> mempty { 
                  libBuildInfo = mempty {
                    ccOptions = llvmCppFlags,
                    extraLibs = ["LLVM-" ++ libraryVersion]
                   }
                 }
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
       oldGhcRts <- lookupEnv v
       setEnv v (maybe id (\o n -> o ++ " " ++ n) oldGhcRts "-K32M")
       haddockHook simpleUserHooks packageDescription localBuildInfo userHooks haddockFlags
       maybe (unsetEnv v) (setEnv v) oldGhcRts
   }

