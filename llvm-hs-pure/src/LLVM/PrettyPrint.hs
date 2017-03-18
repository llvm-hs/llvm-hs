{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE
  TemplateHaskell,
  GeneralizedNewtypeDeriving
  #-}
-- | Tools for printing out AST.'LLVM.AST.Module' code so that it's actually useful.
module LLVM.PrettyPrint (
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

import LLVM.Prelude

import Data.Monoid
import Data.Map (Map)
import qualified Data.Map as Map
import LLVM.Internal.PrettyPrint

import qualified LLVM.AST as A
import qualified LLVM.AST.DataLayout as A
import qualified LLVM.AST.Constant as A.C
import qualified LLVM.AST.AddrSpace as A
import qualified LLVM.AST.Float as A
import qualified LLVM.AST.FloatingPointPredicate as A
import qualified LLVM.AST.IntegerPredicate as A
import qualified LLVM.AST.FunctionAttribute as A
import qualified LLVM.AST.ParameterAttribute as A  
import qualified LLVM.AST.CallingConvention as A
import qualified LLVM.AST.Visibility as A
import qualified LLVM.AST.DLL as A.DLL
import qualified LLVM.AST.COMDAT as A.COMDAT
import qualified LLVM.AST.Linkage as A
import qualified LLVM.AST.ThreadLocalStorage as A.TLS
import qualified LLVM.AST.InlineAssembly as A
import qualified LLVM.AST.RMWOperation as A

fmap concat $ mapM makePrettyShowInstance [
  ''A.Module,
  ''A.Definition,
  ''A.DataLayout,
  ''A.Operand,
  ''A.MetadataNodeID,
  ''A.MetadataNode,
  ''A.Metadata,
  ''A.Type,
  ''A.Name,
  ''A.Global,
  ''A.UnnamedAddr,
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
  ''A.DLL.StorageClass,
  ''A.COMDAT.SelectionKind,
  ''A.Linkage,
  ''A.SomeFloat,
  ''A.Named,
  ''A.Terminator,
  ''A.TailCallKind,
  ''A.Instruction,
  ''A.LandingPadClause,
  ''A.InlineAssembly,
  ''A.RMWOperation,
  ''A.Dialect,
  ''A.FastMathFlags,
  ''A.SynchronizationScope,
  ''A.MemoryOrdering,
  ''A.GroupID,
  ''A.TLS.Model,
  ''Either,
  ''Maybe
 ]

-- | Show an AST.'LLVM.AST.Module' or part thereof both with qualified
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

-- | a 'PrefixScheme' for types not of llvm-hs, but nevertheless used
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
  ("LLVM.AST", Nothing),
  ("LLVM.AST.AddrSpace", Nothing),
  ("LLVM.AST.DataLayout", Nothing),
  ("LLVM.AST.Float", Nothing),
  ("LLVM.AST.InlineAssembly", Nothing),
  ("LLVM.AST.Instruction", Nothing),
  ("LLVM.AST.Name", Nothing),
  ("LLVM.AST.Operand", Nothing),
  ("LLVM.AST.Type", Nothing),
  ("LLVM.AST.FloatingPointPredicate", Just "FPred"),
  ("LLVM.AST.IntegerPredicate", Just "IPred"),
  ("LLVM.AST.Constant", Just "C"),
  ("LLVM.AST.Attribute", Just "A"),
  ("LLVM.AST.Global", Just "G"),
  ("LLVM.AST.CallingConvention", Just "CC"),
  ("LLVM.AST.Visibility", Just "V"),
  ("LLVM.AST.DLL", Just "DLL"),
  ("LLVM.AST.COMDAT", Just "COMDAT"),
  ("LLVM.AST.Linkage", Just "L")
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
