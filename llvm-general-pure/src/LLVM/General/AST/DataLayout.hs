-- | <http://llvm.org/docs/LangRef.html#data-layout>
module LLVM.General.AST.DataLayout where

import Data.Data
import Data.Word

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)

import LLVM.General.AST.AddrSpace

-- | Little Endian is the one true way :-). Sadly, we must support the infidels.
data Endianness = LittleEndian | BigEndian
  deriving (Eq, Ord, Read, Show, Typeable, Data)

-- | An AlignmentInfo describes how a given type must and would best be aligned
data AlignmentInfo = AlignmentInfo {
    abiAlignment :: Word32,
    preferredAlignment :: Maybe Word32
  }
  deriving (Eq, Ord, Read, Show, Typeable, Data)

-- | A type of type for which 'AlignmentInfo' may be specified
data AlignType
  = IntegerAlign
  | VectorAlign
  | FloatAlign
  | AggregateAlign
  deriving (Eq, Ord, Read, Show, Typeable, Data)

-- | A style of name mangling
data Mangling
  = ELFMangling
  | MIPSMangling
  | MachOMangling
  | WindowsCOFFMangling
  deriving (Eq, Ord, Read, Show, Typeable, Data)

-- | a description of the various data layout properties which may be used during
-- optimization
data DataLayout = DataLayout {
    endianness :: Maybe Endianness,
    mangling :: Maybe Mangling,
    stackAlignment :: Maybe Word32,
    pointerLayouts :: Map AddrSpace (Word32, AlignmentInfo),
    typeLayouts :: Map (AlignType, Word32) AlignmentInfo,
    nativeSizes :: Maybe (Set Word32)
  }
  deriving (Eq, Ord, Read, Show, Typeable, Data)

-- | a 'DataLayout' which specifies nothing
defaultDataLayout = DataLayout {
  endianness = Nothing,
  mangling = Nothing,
  stackAlignment = Nothing,
  pointerLayouts = Map.empty,
  typeLayouts = Map.empty,
  nativeSizes = Nothing
 }

