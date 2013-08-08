import Control.Exception (SomeException, try)
import Control.Monad
import Data.Maybe
import Data.List (isPrefixOf, (\\), intercalate, stripPrefix)
import Data.Map (Map)
import qualified Data.Map as Map
import Distribution.Simple
import Distribution.Simple.Program
import Distribution.Simple.Setup hiding (Flag)
import Distribution.Simple.LocalBuildInfo
import Distribution.PackageDescription
import Distribution.Version
import System.Environment
import System.SetEnv
import Distribution.System

main = defaultMainWithHooks simpleUserHooks {
    haddockHook = \packageDescription localBuildInfo userHooks haddockFlags -> do
       let v = "GHCRTS"
       oldGhcRts <- try $ getEnv v :: IO (Either SomeException String)
       setEnv v (either (const id) (\o n -> o ++ " " ++ n) oldGhcRts "-K32M")
       haddockHook simpleUserHooks packageDescription localBuildInfo userHooks haddockFlags
       either (const (unsetEnv v)) (setEnv v) oldGhcRts
   }
