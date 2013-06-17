module LLVM.General.AST.DataLayout where

import Data.Word

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)

import LLVM.General.AST.AddrSpace

data Endianness = BigEndian | LittleEndian
  deriving (Eq, Ord, Read, Show)

data AlignmentInfo = AlignmentInfo {
    abiAlignment :: Word32,
    preferredAlignment :: Word32
  }
  deriving (Eq, Ord, Read, Show)

data AlignType
  = IntegerAlign
  | VectorAlign
  | FloatAlign
  | AggregateAlign
  | StackAlign
  deriving (Eq, Ord, Read, Show)

data DataLayout = DataLayout {
    endianness :: Maybe Endianness,
    stackAlignment :: Maybe Word32,
    pointerLayouts :: Map AddrSpace (Word32, AlignmentInfo),
    typeLayouts :: Map (AlignType, Word32) AlignmentInfo,
    nativeSizes :: Maybe (Set Word32)
  }
  deriving (Eq, Ord, Read, Show)

defaultDataLayout = DataLayout {
  endianness = Nothing,
  stackAlignment = Nothing,
  pointerLayouts = Map.empty,
  typeLayouts = Map.empty,
  nativeSizes = Nothing
 }

