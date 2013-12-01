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
import System.IO
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

llvmVersion = "svn"
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
  () <- cmd (Cwd p) c a
{-
  liftIO $ do
    putStrLn $ "out:\n" ++ out
    putStrLn $ "err:\n" ++ err
-}
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

stamp :: ((String, String)) -> String
stamp (stage, pkg) = pkg </> "dist/shake/stamps" </> stage

parseStamp :: String -> (String, String)
parseStamp s = (takeFileName s, takeDirectory1 s)

needStamps :: [(String,String)] -> Action ()
needStamps ls = need (map stamp ls)

main = shake shakeOptions { 
         shakeVersion = "2",
         shakeVerbosity = Normal
       } $ do

  action $ do
    liftIO $ hSetBuffering stdout NoBuffering
    liftIO $ hSetBuffering stderr NoBuffering

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
      allPkgs = [ "llvm-general-pure", "llvm-general"]
      needStage stage pkgs = needStamps [ (stage, pkg) | pkg <- pkgs ]

  phony "build" $ needStage "built" allPkgs
  phony "test" $ needStage "tested" allPkgs
  phony "doc" $ needStage "documented" allPkgs
  phony "pubdoc" $ needStage "docPublished" allPkgs

  stamp ("*","*") *> \(stmp@(parseStamp -> (stage, pkg))) -> (>> touch stmp) $ do
    cabalStep <- getCabalStep
    tag <- getCabalVersion (CabalVersion pkg)
    buildRoot <- getBuildRoot (BuildRoot ())
    case stage of
      "configured" -> do
        needStage "installed" (localPackageDeps pkg)
        need [ pkg </> "Setup.hs" ]
        need [ pkg </> pkg ++ ".cabal" ]
        cabalStep pkg $ [ "install-deps", "--enable-tests" ] ++ shared
        cabalStep pkg $ [ "configure", "--enable-tests" {- , "-fshared-llvm" -} ] ++ shared

      "built" -> do
        needStage "configured" [pkg]
        needRecursive $ pkg </> "src"
        needRecursive $ pkg </> "test"
        cabalStep pkg [ "build" ]

      "installed" -> do
        needStage "built" [pkg]
        () <- cmd "mv" [ pkg </> "dist", pkg </> "dist-hold" ]
        () <- cmd "cp" [ "-r", pkg </> "dist-hold", pkg </> "dist" ]
        cabalStep pkg $ [ "install", "--reinstall", "--force-reinstalls" ] ++ shared
        () <- cmd "rm" [ "-r", pkg </> "dist" ]
        () <- cmd "mv" [ pkg </> "dist-hold", pkg </> "dist" ]
        return ()

      "tested" -> do
        needStage "built" [pkg]
        cabalStep pkg [ "test" ]

      "documented" -> do
        needStage "built" [pkg]
        needStage "documented" (localPackageDeps pkg)
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
    
      "docPublished" -> do
        needStage "documented" [pkg]
        ghPagesExists <- doesDirectoryExist ghPages
        unless ghPagesExists $ cmd (Cwd "out") "git" ["clone", buildRoot, "-b", "gh-pages", "gh-pages"]
        () <- cmd "rm" [ "-rf", ghPages </> tag </> "doc" </> "html" </> pkg ]
        () <- cmd "mkdir" [ "-p", ghPages </> tag </> "doc" </> "html" ]
        () <- cmd "cp" [ "-r", pkg </> "dist/doc/html" </> pkg, ghPages </> tag </> "doc" </> "html" ]
        () <- cmd (Cwd ghPages) "git" [ "add", "-A", "." ]
        () <- cmd (Cwd ghPages) "git" [ "commit", "-m", show ("update " ++ tag ++ " " ++ pkg ++ " doc") ]
        () <- cmd (Cwd ghPages) "git" [ "push" ]
        return ()

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


