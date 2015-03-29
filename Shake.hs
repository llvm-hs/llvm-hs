{-# 
  LANGUAGE 
  MultiParamTypeClasses,
  GeneralizedNewtypeDeriving,
  StandaloneDeriving,
  DeriveDataTypeable,
  FlexibleInstances,
  ViewPatterns
  #-}
import Prelude hiding ((*>))
import Control.Exception
import Control.Monad
import Control.DeepSeq
import Data.Typeable
import Data.Functor
import Data.List
import Data.Hashable
import Data.Binary
import Development.Shake hiding (command_)
import qualified Development.Shake as S (command_)
import Development.Shake.FilePath
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

llvmVersion = "3.5.1"
llvmDir = "out" </> ("llvm-" ++ llvmVersion)
pkgName = "llvm-general"

wipedir :: FilePath -> Action ()
wipedir d = do
  present <- doesDirectoryExist d
  when present $ command_ [] "rm" [ "-r", d ]

mkdir :: FilePath -> Action ()
mkdir d = command_ [] "mkdir" [ "-p", d ]

untar :: FilePath -> FilePath -> Action ()
untar d tb = command_ [] "tar" [ "xCf", d, tb ]

needRecursive :: FilePath -> Action ()
needRecursive p = do
  subdirs <- getDirectoryDirs p
  forM [ s | s <- subdirs, s /= ".git" ] $ needRecursive . (p </>)
  files <- getDirectoryFiles p ["*"]
  need $ map (p </>) files

touch f = do
  let d = dropFileName f
  dExists <- doesDirectoryExist d
  unless dExists $ command_ [] "mkdir" ["-p", d]
  command_ [] "touch" [ f ]

command_ :: [CmdOption] -> String -> [String] -> Action ()
command_ opts c args = foldr x (S.command_ opts c args) opts
  where x (Cwd p') rest = do
          p <- liftIO $ canonicalizePath p'
          putQuiet $ "Entering directory `" ++ p ++ "'"
          rest
          putQuiet $ "Leaving directory `" ++ p ++ "'"
        x _ rest = rest

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

  ghPagesLock <- newResource "gh-pages lock" 1

  action $ do
    args <- liftIO getArgs
    need (if null args then [ stamp ("tested", "llvm-general") ] else args)

  phony "env" $ do
    command_ [] "env" []

  let shared = [ "--enable-shared" | True ]
  let ghPages = "out" </> "gh-pages"
      sandbox = "out" </> "sandbox"
      sandboxConfigFile = "out" </> "cabal.sandbox.config"

  let getCabalStep = do
        buildRoot <- getBuildRoot (BuildRoot ())
        ownLLVM <- getLlvmConfig (LLVMConfig ())
        pathOpt <- if ownLLVM 
                    then do
                      opt <- addPath [buildRoot </> llvmDir </> "install/bin"] []
                      return [opt]
                     else 
                      return []
        return $ \pkg args -> command_ ([Cwd pkg] ++ pathOpt) "cabal" $ [ "--sandbox-config-file=" ++ (buildRoot </> sandboxConfigFile) ] ++ args

  let localPackageDeps "llvm-general" = [ "llvm-general-pure" ]
      localPackageDeps _ = []
      allPkgs = [ "llvm-general-pure", "llvm-general"]
      needStage stage pkgs = needStamps [ (stage, pkg) | pkg <- pkgs ]

  let cabal args = do
        command_ [] "cabal" $ [ "--sandbox-config-file=" ++ sandboxConfigFile ] ++ args

  let ensureSandbox = do
        present <- doesFileExist sandboxConfigFile
        unless present $ do
          cabal [ "sandbox", "init", "--sandbox=" ++ sandbox ]
          cabal $ [ "sandbox", "add-source" ] ++ allPkgs

  phony "build" $ needStage "built" allPkgs
  phony "test" $ needStage "tested" allPkgs
  phony "doc" $ needStage "documented" allPkgs
  phony "pubdoc" $ needStage "docPublished" allPkgs

  stamp ("*","*") *> \(stmp@(parseStamp -> (stage, pkg))) -> (>> touch stmp) $ do
    ensureSandbox
    cabalStep <- getCabalStep
    tag <- getCabalVersion (CabalVersion pkg)
    buildRoot <- getBuildRoot (BuildRoot ())
    case stage of
      "configured" -> do
        needStage "installed" (localPackageDeps pkg)
        need [ pkg </> "Setup.hs" ]
        need [ pkg </> pkg ++ ".cabal" ]
        cabalStep pkg $ [ "install", "--only-dependencies", "--enable-tests" ] ++ shared
        cabalStep pkg $ [ "configure", "--enable-tests", "-fshared-llvm" ] ++ shared

      "built" -> do
        needStage "configured" [pkg]
        needRecursive $ pkg </> "src"
        needRecursive $ pkg </> "test"
        cabalStep pkg [ "build" ]

      "installed" -> do
        needStage "built" [pkg]
        cabalStep pkg $ [ "install", "--reinstall", "--force-reinstalls" ] ++ shared

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
        withResource ghPagesLock 1 $ do
          ghPagesExists <- doesDirectoryExist ghPages
          unless ghPagesExists $ command_ [Cwd "out"] "git" ["clone", buildRoot, "-b", "gh-pages", "gh-pages"]
          command_ [] "rm" [ "-rf", ghPages </> tag </> "doc" </> "html" </> pkg ]
          command_ [] "mkdir" [ "-p", ghPages </> tag </> "doc" </> "html" ]
          command_ [] "cp" [ "-r", pkg </> "dist/doc/html" </> pkg, ghPages </> tag </> "doc" </> "html" ]
          command_ [Cwd ghPages] "git" [ "add", "-A", "." ]
          command_ [Cwd ghPages] "git" [ "commit", "-m", show ("update " ++ tag ++ " " ++ pkg ++ " doc") ]
          command_ [Cwd ghPages] "git" [ "push" ]

  llvmDir </> "install/bin/llvm-config" *> \out -> do
    [tarball] <- getDirectoryFiles "." [ "downloads/llvm-" ++ llvmVersion ++ ".src.tar.*" ]
    buildRoot <- askOracle (BuildRoot ())
    need [ tarball ]
    let buildDir = llvmDir </> "build"
    wipedir buildDir
    mkdir buildDir
    untar buildDir tarball
    (".":"..":[srcDir']) <- liftM sort $ liftIO $ System.Directory.getDirectoryContents buildDir
    let srcDir = buildDir </> srcDir'
    command_ [Cwd srcDir] "sh" [
        "./configure",
        "--prefix=" ++ buildRoot </> llvmDir </> "install",
        "--enable-shared"
        ]
    command_ [Cwd srcDir] "make" [ "-j", "8", "install" ]
    wipedir buildDir


