{-# LANGUAGE
  TemplateHaskell,
  GeneralizedNewtypeDeriving
  #-}
-- | Tools for printing out AST.'LLVM.General.AST.Module' code so that it's actually useful.
module LLVM.General.PrettyPrint (
  PrettyShow(..),
  showPretty,
  showPrettyEx,
  PrefixScheme(..),
  shortPrefixScheme,
  longPrefixScheme,
  defaultPrefixScheme,
  basePrefixScheme, 
  shortASTPrefixScheme,
  longASTPrefixScheme,
  imports
  ) where

import LLVM.General.Prelude

import Data.Monoid
import Data.Map (Map)
import qualified Data.Map as Map
import LLVM.General.Internal.PrettyPrint

import qualified LLVM.General.AST as A
import qualified LLVM.General.AST.DataLayout as A
import qualified LLVM.General.AST.Constant as A.C
import qualified LLVM.General.AST.AddrSpace as A
import qualified LLVM.General.AST.Float as A
import qualified LLVM.General.AST.FloatingPointPredicate as A
import qualified LLVM.General.AST.IntegerPredicate as A
import qualified LLVM.General.AST.Attribute as A
import qualified LLVM.General.AST.CallingConvention as A
import qualified LLVM.General.AST.Visibility as A
import qualified LLVM.General.AST.Linkage as A
import qualified LLVM.General.AST.InlineAssembly as A
import qualified LLVM.General.AST.RMWOperation as A

fmap concat $ mapM makePrettyShowInstance [
  ''A.Module,
  ''A.Definition,
  ''A.DataLayout,
  ''A.Operand,
  ''A.MetadataNodeID,
  ''A.MetadataNode,
  ''A.Type,
  ''A.Name,
  ''A.Global,
  ''A.AlignmentInfo,
  ''A.AlignType,
  ''A.Mangling,
  ''A.C.Constant,
  ''A.AddrSpace,
  ''A.Endianness,
  ''A.BasicBlock,
  ''A.FloatingPointPredicate,
  ''A.IntegerPredicate,
  ''A.FloatingPointFormat,
  ''A.FunctionAttribute,
  ''A.ParameterAttribute,
  ''A.Parameter,
  ''A.CallingConvention,
  ''A.Visibility,
  ''A.Linkage,
  ''A.SomeFloat,
  ''A.Named,
  ''A.Terminator,
  ''A.Instruction,
  ''A.LandingPadClause,
  ''A.InlineAssembly,
  ''A.RMWOperation,
  ''A.Dialect,
  ''A.FastMathFlags,
  ''A.SynchronizationScope,
  ''A.MemoryOrdering,
  ''A.GroupID,
  ''Either,
  ''Maybe
 ]

-- | Show an AST.'LLVM.General.AST.Module' or part thereof both with qualified
-- identifiers (resolving some ambiguity, making the output usable Haskell) and
-- with indentation (making the output readable).
showPretty :: PrettyShow a => a -> String
showPretty = showPrettyEx 80 "  " defaultPrefixScheme

-- | Like 'showPretty' but allowing configuration of the format
showPrettyEx
  :: PrettyShow a
  => Int -- ^ line length before attempting breaking
  -> String -- ^ one unit of indentation
  -> PrefixScheme -- ^ prefixes to use for qualifying names
  -> a
  -> String
showPrettyEx width indent (PrefixScheme ps) = renderEx width indent (defaultPrettyShowEnv { prefixes = ps }) . prettyShow

-- | A 'PrefixScheme' is a mapping between haskell module names and
-- the prefixes with which they should be rendered when printing code.
newtype PrefixScheme = PrefixScheme (Map String (Maybe String))
  deriving (Eq, Ord, Read, Show, Monoid)

-- | a 'PrefixScheme' for types not of llvm-general, but nevertheless used
-- in the AST. Useful for building other 'PrefixScheme's.
basePrefixScheme :: PrefixScheme
basePrefixScheme = PrefixScheme $ Map.fromList [
  ("Data.Maybe", Nothing),
  ("Data.Either", Nothing),
  ("Data.Map", Just "Map"),
  ("Data.Set", Just "Set"),
  ("GHC.Base", Nothing)
 ]

-- | a terse 'PrefixScheme' for types in the AST, leaving most names unqualified.
-- Useful for building other 'PrefixScheme's. If you think you want to use this, you
-- probably want 'shortPrefixScheme' instead.
shortASTPrefixScheme :: PrefixScheme
shortASTPrefixScheme = PrefixScheme $ Map.fromList [
  ("LLVM.General.AST", Nothing),
  ("LLVM.General.AST.AddrSpace", Nothing),
  ("LLVM.General.AST.DataLayout", Nothing),
  ("LLVM.General.AST.Float", Nothing),
  ("LLVM.General.AST.InlineAssembly", Nothing),
  ("LLVM.General.AST.Instruction", Nothing),
  ("LLVM.General.AST.Name", Nothing),
  ("LLVM.General.AST.Operand", Nothing),
  ("LLVM.General.AST.Type", Nothing),
  ("LLVM.General.AST.FloatingPointPredicate", Just "FPred"),
  ("LLVM.General.AST.IntegerPredicate", Just "IPred"),
  ("LLVM.General.AST.Constant", Just "C"),
  ("LLVM.General.AST.Attribute", Just "A"),
  ("LLVM.General.AST.Global", Just "G"),
  ("LLVM.General.AST.CallingConvention", Just "CC"),
  ("LLVM.General.AST.Visibility", Just "V"),
  ("LLVM.General.AST.Linkage", Just "L")
 ]

-- | a conservative 'PrefixScheme' for types in the AST which qualifies everything.
-- Useful for building other 'PrefixScheme's. If you think you want to use this, you
-- probably want 'longPrefixScheme' instead.
longASTPrefixScheme :: PrefixScheme
longASTPrefixScheme = case shortASTPrefixScheme of
  PrefixScheme m -> PrefixScheme $ maybe (Just "A") (Just . ("A."++)) <$> m

-- | a terse 'PrefixScheme', leaving most names unqualified.
shortPrefixScheme :: PrefixScheme
shortPrefixScheme = shortASTPrefixScheme <> basePrefixScheme

-- | a conservative 'PrefixScheme' which qualifies everything.
longPrefixScheme :: PrefixScheme
longPrefixScheme = longASTPrefixScheme <> basePrefixScheme

-- | the default 'PrefixScheme' used by 'showPretty'
defaultPrefixScheme :: PrefixScheme
defaultPrefixScheme = longPrefixScheme

-- | print Haskell imports to define the correct prefixes for use with the output
-- of a given 'PrefixScheme'.
imports :: PrefixScheme -> String
imports (PrefixScheme p) = unlines [
  "import " ++ maybe mod (\abbr -> "qualified " ++ mod ++ " as " ++ abbr) mAbbr
  | (mod, mAbbr) <- Map.toList p, mod /= "GHC.Base"
 ]


