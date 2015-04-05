-- | <http://llvm.org/docs/LangRef.html#data-layout>
module LLVM.General.AST.DataLayout where

import LLVM.General.Prelude

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
    endianness :: Endianness,
    mangling :: Maybe Mangling,
    stackAlignment :: Maybe Word32,
    pointerLayouts :: Map AddrSpace (Word32, AlignmentInfo),
    typeLayouts :: Map (AlignType, Word32) AlignmentInfo,
    aggregateLayout :: AlignmentInfo,
    nativeSizes :: Maybe (Set Word32)
  }
  deriving (Eq, Ord, Read, Show, Typeable, Data)

-- | a default 'DataLayout'
defaultDataLayout endianness = DataLayout {
  endianness = endianness,
  mangling = Nothing,
  stackAlignment = Nothing,
  pointerLayouts = Map.fromList [
    (AddrSpace 0, (64, AlignmentInfo 64 (Just 64)))
   ],
  typeLayouts = Map.fromList [
    ((IntegerAlign, 1), AlignmentInfo 8 (Just 8)),
    ((IntegerAlign, 8), AlignmentInfo 8 (Just 8)),
    ((IntegerAlign, 16), AlignmentInfo 16 (Just 16)),
    ((IntegerAlign, 32), AlignmentInfo 32 (Just 32)),
    ((IntegerAlign, 64), AlignmentInfo 32 (Just 64)),
    ((FloatAlign, 16), AlignmentInfo 16 (Just 16)),
    ((FloatAlign, 32), AlignmentInfo 32 (Just 32)),
    ((FloatAlign, 64), AlignmentInfo 64 (Just 64)),
    ((FloatAlign, 128), AlignmentInfo 128 (Just  128)),
    ((VectorAlign, 64), AlignmentInfo 64 (Just 64)),
    ((VectorAlign, 128), AlignmentInfo 128 (Just 128))
   ],
  aggregateLayout = AlignmentInfo 0 (Just 64),
  nativeSizes = Nothing
 }

