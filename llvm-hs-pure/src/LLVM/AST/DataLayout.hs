-- | <http://llvm.org/docs/LangRef.html#data-layout>
module LLVM.AST.DataLayout where

import LLVM.Prelude

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)

import LLVM.AST.AddrSpace

-- | Little Endian is the one true way :-). Sadly, we must support the infidels.
data Endianness = LittleEndian | BigEndian
  deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)

-- | An AlignmentInfo describes how a given type must and would best be aligned
data AlignmentInfo = AlignmentInfo {
    abiAlignment :: Word32,
    preferredAlignment :: Word32
  }
  deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)

-- | A type of type for which 'AlignmentInfo' may be specified
data AlignType
  = IntegerAlign
  | VectorAlign
  | FloatAlign
  deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)

-- | A style of name mangling
data Mangling
  = ELFMangling
  | MIPSMangling
  | MachOMangling
  | WindowsCOFFMangling
  deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)

-- | a description of the various data layout properties which may be used during
-- optimization
data DataLayout = DataLayout {
    endianness :: Endianness,
    mangling :: Maybe Mangling,
    stackAlignment :: Maybe Word32,
    pointerLayouts :: Map AddrSpace (Word32, AlignmentInfo),
    typeLayouts :: Map (AlignType, Word32) AlignmentInfo,
    aggregateLayout :: AlignmentInfo,
    nativeSizes :: Maybe (Set Word32)
  }
  deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)

-- | a default 'DataLayout'
defaultDataLayout :: Endianness -> DataLayout
defaultDataLayout defaultEndianness = DataLayout {
  endianness = defaultEndianness,
  mangling = Nothing,
  stackAlignment = Nothing,
  pointerLayouts = Map.fromList [
    (AddrSpace 0, (64, AlignmentInfo 64 64))
   ],
  typeLayouts = Map.fromList [
    ((IntegerAlign, 1), AlignmentInfo 8 8),
    ((IntegerAlign, 8), AlignmentInfo 8 8),
    ((IntegerAlign, 16), AlignmentInfo 16 16),
    ((IntegerAlign, 32), AlignmentInfo 32 32),
    ((IntegerAlign, 64), AlignmentInfo 32 64),
    ((FloatAlign, 16), AlignmentInfo 16 16),
    ((FloatAlign, 32), AlignmentInfo 32 32),
    ((FloatAlign, 64), AlignmentInfo 64 64),
    ((FloatAlign, 128), AlignmentInfo 128 128),
    ((VectorAlign, 64), AlignmentInfo 64 64),
    ((VectorAlign, 128), AlignmentInfo 128 128)
   ],
  aggregateLayout = AlignmentInfo 0 64,
  nativeSizes = Nothing
 }

