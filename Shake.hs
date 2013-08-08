{-# 
  LANGUAGE 
  MultiParamTypeClasses,
  GeneralizedNewtypeDeriving,
  StandaloneDeriving,
  DeriveDataTypeable,
  FlexibleInstances,
  ViewPatterns
  #-}
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
import Distribution.Package hiding (pkgName)
import Distribution.PackageDescription
import Distribution.PackageDescription.Parse
import Distribution.PackageDescription.PrettyPrint
import Distribution.PackageDescription.Configuration
import Text.Parsec
import Text.Parsec.String
import Data.Version

llvmVersion = "3.4svn"
llvmDir = "out" </> ("llvm-" ++ llvmVersion)
pkgName = "llvm-general"

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

touch f = do
  let d = dropFileName f
  dExists <- doesDirectoryExist d
  unless dExists $ cmd "mkdir" ["-p", d]
  cmd "touch" [ f ]

systemCwdV p' c a = do
  p <- liftIO $ canonicalizePath p'
  putQuiet $ "Entering directory `" ++ p ++ "'"
  (Stdout out, Stderr err) <- cmd (Cwd p) c a
  liftIO $ do
    putStrLn $ "out:\n" ++ out
    putStrLn $ "err:\n" ++ err
  putQuiet $ "Leaving directory `" ++ p ++ "'"

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
newtype LLVMConfig = LLVMConfig () deriving (Eq, Ord, Read, Show, Binary, Hashable, NFData, Typeable)
newtype CabalVersion = CabalVersion String deriving (Eq, Ord, Read, Show, Binary, Hashable, NFData, Typeable)

main = shake shakeOptions { 
         shakeVersion = "2",
         shakeVerbosity = Normal
       } $ do

  getBuildRoot <- addOracle $ \(BuildRoot _) -> do
    liftIO $ getWorkingDirectory

  getCabalVersion <- addOracle $ \(CabalVersion pkg) -> do
    liftIO $ liftM (showVersion . pkgVersion . package . packageDescription) $ readPackageDescription silent (pkg </> pkg ++ ".cabal")

  getLlvmConfig <- addOracle $ \(LLVMConfig _) -> do
    Exit exitCode <- command [] "which" ["llvm-config"]
    let x = exitCode /= ExitSuccess
    when x $ do
      done <- doesFileExist (llvmDir </> "install/bin/llvm-config")
      unless done $ need [ (llvmDir </> "install/bin/llvm-config") ]
    return x

  let stamp p s = "out" </> "stamps" </> p </> s
      stampPkg = takeFileName . takeDirectory

  action $ do
    args <- liftIO getArgs
    need args

  let getCabalStep = do
        buildRoot <- getBuildRoot (BuildRoot ())
        ownLLVM <- getLlvmConfig (LLVMConfig ())
        let subBuildEnv = if ownLLVM 
                           then withAlteredEnvVar "PATH" (prefixPathVar $ buildRoot </> llvmDir </> "install/bin")
                           else id
        return $ \pkg args -> subBuildEnv (systemCwdV pkg "cabal-dev" $ [ "--sandbox", buildRoot </> "cabal-dev"] ++ args)

  let shared = [ "--enable-shared" | True ]
  let ghPages = "out" </> "gh-pages"
  let localPackageDeps "llvm-general" = [ "llvm-general-pure" ]
      localPackageDeps _ = []

  stamp "*" "configured" *> \(stamp'@(stampPkg -> pkg)) -> do
    cabalStep <- getCabalStep
    need [ stamp dpkg "installed" | dpkg <- localPackageDeps pkg ]
    need [ pkg </> pkg ++ ".cabal" ]
    cabalStep pkg $ [ "install-deps", "--enable-tests" ] ++ shared
    cabalStep pkg $ [ "configure", "--enable-tests" {- , "-fshared-llvm" -} ] ++ shared
    touch stamp'

  phony "build" $ need [ stamp "llvm-general" "built" ]
  stamp "*" "built" *> \(stamp'@(stampPkg -> pkg)) -> do
    cabalStep <- getCabalStep
    need [ stamp pkg "configured" ]
    needRecursive "llvm-general/src"
    needRecursive "llvm-general/test"              
    cabalStep pkg [ "build" ]
    touch stamp'

  stamp "*" "installed" *> \(stamp'@(stampPkg -> pkg)) -> do
    cabalStep <- getCabalStep
    need [ stamp pkg "built" ]
    cabalStep pkg $ [ "install" ] ++ shared
    touch stamp'

  phony "test" $ do
    need [ stamp pkg "tested" | pkg <- ["llvm-general-pure", "llvm-general"] ]

  stamp "*" "tested" *> \(stamp'@(stampPkg -> pkg)) -> do
    cabalStep <- getCabalStep
    need [ stamp pkg  "built" ]
    cabalStep pkg [ "test" ]
    touch stamp'

  phony "doc" $ need [ stamp "llvm-general" "documented" ]
  stamp "*" "documented" *> \(stamp'@(stampPkg -> pkg)) -> do
    buildRoot <- getBuildRoot (BuildRoot ())
    tag <- getCabalVersion (CabalVersion pkg)
    need [ stamp pkg "built" ]
    need [ stamp dpkg "documented" | dpkg <- localPackageDeps pkg ]
    cabalStep <- getCabalStep
    cabalStep pkg $ [
        "haddock",
        "--html-location=http://hackage.haskell.org/packages/archive/$pkg/$version/doc/html"
      ] ++ [
        "--haddock-options=--read-interface="
        ++ ("/llvm-general" </> tag </> "doc/html" </> dpkg)
        ++ ","
        ++ (buildRoot </> dpkg </> "dist/doc/html" </> dpkg </> dpkg <.> "haddock")
        | dpkg <- localPackageDeps pkg
      ]
    touch stamp'
    
  phony "pubdoc" $ need [ stamp pkg "docPublished" | pkg <- ["llvm-general-pure", "llvm-general"] ]
  stamp "*" "docPublished" *> \(stamp'@(stampPkg -> pkg)) -> do
    need [ stamp pkg "documented" ]
    buildRoot <- getBuildRoot (BuildRoot ())
    ghPagesExists <- doesDirectoryExist ghPages
    tag <- getCabalVersion (CabalVersion pkg)
    unless ghPagesExists $ cmd (Cwd "out") "git" ["clone", buildRoot, "-b", "gh-pages", "gh-pages"]
    () <- cmd "rm" [ "-rf", ghPages </> tag </> "doc" </> "html" </> pkg ]
    () <- cmd "mkdir" [ "-p", ghPages </> tag </> "doc" </> "html" ]
    () <- cmd "cp" [ "-r", pkg </> "dist/doc/html" </> pkg, ghPages </> tag </> "doc" </> "html" ]
    () <- cmd (Cwd ghPages) "git" [ "add", "-A", "." ]
    () <- cmd (Cwd ghPages) "git" [ "commit", "-m", show ("update " ++ tag ++ " " ++ pkg ++ " doc") ]
    () <- cmd (Cwd ghPages) "git" [ "push" ]
    touch stamp'

  llvmDir </> "install/bin/llvm-config" *> \out -> do
    let tarball = "downloads/llvm-" ++ llvmVersion ++ ".src.tar.gz"
    buildRoot <- askOracle (BuildRoot ())
    need [ tarball ]
    let buildDir = llvmDir </> "build"
    wipedir buildDir
    mkdir buildDir
    untar buildDir tarball
    (".":"..":[srcDir']) <- liftM sort $ liftIO $ System.Directory.getDirectoryContents buildDir
    let srcDir = buildDir </> srcDir'
    systemCwdV srcDir "sh" [
        "./configure",
        "--prefix=" ++ buildRoot </> llvmDir </> "install",
        "--enable-shared"
        ]
    systemCwdV srcDir "make" [ "-j", "8", "install" ]
    wipedir buildDir


