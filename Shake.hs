{-# 
  LANGUAGE 
  MultiParamTypeClasses,
  GeneralizedNewtypeDeriving,
  StandaloneDeriving,
  DeriveDataTypeable,
  FlexibleInstances
  #-}
import Debug.Trace
import Control.Exception
import Control.Monad
import Control.DeepSeq
import Data.Typeable
import Data.Functor
import Data.List
import Data.Hashable
import Data.Binary
import Development.Shake
import Development.Shake.FilePath
import Development.Shake.Command
import System.FilePath.Posix hiding ((</>), doesDirectoryExist)
import System.Posix.Directory
import System.Posix.Env hiding (getEnv)
import System.Directory hiding (doesDirectoryExist, doesFileExist)
import System.Environment (getArgs)
import System.Exit
import Distribution.Verbosity
import Distribution.Text (simpleParse)
import Distribution.Compiler
import Distribution.Package
import Distribution.PackageDescription
import Distribution.PackageDescription.Parse
import Distribution.PackageDescription.Configuration
import Text.Parsec
import Text.Parsec.String
import Data.Version

llvmVersion = "3.2"

wipedir :: FilePath -> Action ()
wipedir d = do
  present <- doesDirectoryExist d
  if present then system' "rm" [ "-r", d ] else return ()

mkdir :: FilePath -> Action ()
mkdir d = system' "mkdir" [ "-p", d ]

untar :: FilePath -> FilePath -> Action ()
untar d tb = system' "tar" [ "zxCf", d, tb ]

needRecursive :: FilePath -> Action ()
needRecursive p = do
  subdirs <- getDirectoryDirs p
  forM [ s | s <- subdirs, s /= ".git" ] $ needRecursive . (p </>)
  files <- getDirectoryFiles p ["*"]
  need $ map (p </>) files

systemCwdV p' c a = do
  p <- liftIO $ canonicalizePath p'
  putQuiet $ "Entering directory `" ++ p ++ "'"
  systemCwd p c a
  putQuiet $ "Leaving directory `" ++ p ++ "'"

{-
getDirectoryFilesRecursive :: FilePath -> FilePattern -> Action [FilePath]
getDirectoryFilesRecursive start pat = do
  locals <- getDirectoryFiles start [pat]
  dirs <- getDirectoryDirs start
  deepers <- forM [ d | d <- dirs, d /= ".git" ] $ \d -> do
    subs <- getDirectoryFilesRecursive (start </> d) pat
    return $ map (d </>) subs
  return $ concat (locals : deepers)

-}
withAlteredEnvVar :: String -> (Maybe String -> Maybe String) -> Action () -> Action ()
withAlteredEnvVar name modify action = do
  let alterEnvVar :: String -> Maybe String -> Action ()
      alterEnvVar name val = liftIO $ maybe (unsetEnv name) (\v -> setEnv name v True) val
  old <- getEnv name
  alterEnvVar name (modify old)
  action
  alterEnvVar name old

setEnvVar = const . Just
prefixPathVar entry = Just . maybe entry ((entry ++ ":") ++) 

newtype BuildRoot = BuildRoot () deriving (Eq, Ord, Read, Show, Binary, Hashable, NFData, Typeable)
newtype NoDoc = NoDoc () deriving (Eq, Ord, Read, Show, Binary, Hashable, NFData, Typeable)
newtype LLVMConfig = LLVMConfig () deriving (Eq, Ord, Read, Show, Binary, Hashable, NFData, Typeable)

main = shake shakeOptions { 
         shakeVersion = "2",
         shakeVerbosity = Normal
       } $ do

  getNoDoc <- addOracle $ \(NoDoc _) -> do
    args <- liftIO getArgs
    return ("--noDoc" `elem` args)

  getBuildRoot <- addOracle $ \(BuildRoot _) -> do
    liftIO $ getWorkingDirectory

  getLlvmConfig <- addOracle $ \(LLVMConfig _) -> do
    Exit exitCode <- command [] "which" ["llvm-config"]
    let x = exitCode /= ExitSuccess
    when x $ do
      done <- doesFileExist "out/install/bin/llvm-config"
      unless done $ need ["out/install/bin/llvm-config"]
    return x

  action $ do
    let pkgName = "llvm-general"
    buildRoot <- getBuildRoot (BuildRoot ())
    ownLLVM <- getLlvmConfig (LLVMConfig ())
    let subBuildEnv = if ownLLVM 
                       then withAlteredEnvVar "PATH" (prefixPathVar $ buildRoot </> "out/install" </> "bin")
                       else id
    let cabalStep args = subBuildEnv $ systemCwdV "." "cabal-dev" args
    need [ pkgName ++ ".cabal" ]
    cabalStep [ "install-deps", "--enable-shared", "--enable-tests" ]
    cabalStep [
      "configure",
--      "--haddock-option", "--use-index=" ++ (buildRoot </> docDir </> "doc-index.html"),
--      "--haddock-option", "--use-contents=" ++ (buildRoot </> docDir </> "index.html"),
      "--haddock-options", "+RTS -K32M -RTS",
      "--ghc-options", "-optl -Wl,-rpath,../lib",
      "--enable-shared",
      "--enable-tests"
     ]
    needRecursive "src"
    cabalStep [ "build" ]
    needRecursive "test"              
    cabalStep [ "test" ]
    noDoc <- getNoDoc (NoDoc ())
    unless noDoc $ cabalStep [ "haddock", "--internal" ]
    cabalStep [ "install" ]
    
    
  "out/install/bin/llvm-config" *> \out -> do
    let tarball = "downloads/llvm-" ++ llvmVersion ++ ".src.tar.gz"
    buildRoot <- askOracle (BuildRoot ())
    need [ tarball ]
    let buildDir = "out/llvm/build"
    wipedir buildDir
    mkdir buildDir
    untar buildDir tarball
    (".":"..":[srcDir']) <- liftM sort $ liftIO $ System.Directory.getDirectoryContents buildDir
    let srcDir = buildDir </> srcDir'
    systemCwdV srcDir "sh" [
        "./configure",
        "--prefix=" ++ buildRoot </> "out/install",
        "--enable-shared"
        ]
    systemCwdV srcDir "make" [ "-j", "8", "install" ]
    wipedir buildDir


