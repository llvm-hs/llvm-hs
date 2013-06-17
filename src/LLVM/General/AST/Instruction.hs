{-# LANGUAGE
  DeriveDataTypeable
  #-}
module LLVM.General.AST.Instruction where

import Data.Data
import Data.Word

import LLVM.General.AST.Type
import LLVM.General.AST.Name
import LLVM.General.AST.Constant
import LLVM.General.AST.Operand
import LLVM.General.AST.IntegerPredicate (IntegerPredicate)
import LLVM.General.AST.FloatingPointPredicate (FloatingPointPredicate)
import LLVM.General.AST.RMWOperation (RMWOperation)
import LLVM.General.AST.CallingConvention (CallingConvention)
import LLVM.General.AST.Attribute (ParameterAttribute, FunctionAttribute)

type InstructionMetadata = [(String, MetadataNode)]

data Terminator 
  = Ret { 
      returnOperand :: Maybe Operand,
      metadata' :: InstructionMetadata
    }
  | CondBr { 
      condition :: Operand, 
      trueDest :: Name, 
      falseDest :: Name,
      metadata' :: InstructionMetadata
    }
  | Br { 
      dest :: Name,
      metadata' :: InstructionMetadata
    }
  | Switch {
      operand0' :: Operand,
      defaultDest :: Name,
      dests :: [(Constant, Name)],
      metadata' :: InstructionMetadata
    }
  | IndirectBr {
      operand0' :: Operand,
      possibleDests :: [Name],
      metadata' :: InstructionMetadata
    }
  | Invoke {
      callingConvention' :: CallingConvention,
      returnAttributes' :: [ParameterAttribute],
      function' :: CallableOperand,
      arguments' :: [(Operand, [ParameterAttribute])],
      functionAttributes' :: [FunctionAttribute],
      returnDest :: Name,
      exceptionDest :: Name,
      metadata' :: InstructionMetadata
    }
  | Resume {
      operand0' :: Operand,
      metadata' :: InstructionMetadata
    }
  | Unreachable {
      metadata' :: InstructionMetadata
    }
  deriving (Eq, Read, Show)

data MemoryOrdering
  = Unordered
  | Monotonic
  | Acquire
  | Release
  | AcquireRelease
  | SequentiallyConsistent
  deriving (Eq, Ord, Read, Show, Data, Typeable)

data Atomicity = Atomicity { 
  crossThread :: Bool, 
  memoryOrdering :: MemoryOrdering
 }
 deriving (Eq, Ord, Read, Show)

data LandingPadClause
    = Catch Constant
    | Filter Constant
    deriving (Eq, Ord, Read, Show)

data Instruction
  = Add { 
      nsw :: Bool,
      nuw :: Bool,
      operand0 :: Operand,
      operand1 :: Operand,
      metadata :: InstructionMetadata
    }
  | FAdd {
      operand0 :: Operand,
      operand1 :: Operand,
      metadata :: InstructionMetadata
    }
  | Sub {
      nsw :: Bool,
      nuw :: Bool,
      operand0 :: Operand,
      operand1 :: Operand,
      metadata :: InstructionMetadata
    }
  | FSub { 
      operand0 :: Operand, 
      operand1 :: Operand, 
      metadata :: InstructionMetadata
    }
  | Mul { 
      nsw :: Bool, 
      nuw :: Bool, 
      operand0 :: Operand, 
      operand1 :: Operand, 
      metadata :: InstructionMetadata 
    }
  | FMul { 
      operand0 :: Operand, 
      operand1 :: Operand, 
      metadata :: InstructionMetadata
    }
  | UDiv { 
      exact :: Bool, 
      operand0 :: Operand, 
      operand1 :: Operand, 
      metadata :: InstructionMetadata
    }
  | SDiv { 
      exact :: Bool, 
      operand0 :: Operand, 
      operand1 :: Operand, 
      metadata :: InstructionMetadata
    }
  | FDiv { 
      operand0 :: Operand, 
      operand1 :: Operand, 
      metadata :: InstructionMetadata
    }
  | URem { 
      operand0 :: Operand, 
      operand1 :: Operand, 
      metadata :: InstructionMetadata
    }
  | SRem { 
      operand0 :: Operand, 
      operand1 :: Operand, 
      metadata :: InstructionMetadata
    }
  | FRem { 
      operand0 :: Operand, 
      operand1 :: Operand, 
      metadata :: InstructionMetadata
    }
  | Shl { 
      nsw :: Bool, 
      nuw :: Bool, 
      operand0 :: Operand, 
      operand1 :: Operand, 
      metadata :: InstructionMetadata
    }
  | LShr { 
      exact :: Bool, 
      operand0 :: Operand, 
      operand1 :: Operand, 
      metadata :: InstructionMetadata
    }
  | AShr { 
      exact :: Bool, 
      operand0 :: Operand, 
      operand1 :: Operand, 
      metadata :: InstructionMetadata
    }
  | And { 
      operand0 :: Operand, 
      operand1 :: Operand, 
      metadata :: InstructionMetadata
    }
  | Or { 
      operand0 :: Operand, 
      operand1 :: Operand, 
      metadata :: InstructionMetadata
    }
  | Xor { 
      operand0 :: Operand, 
      operand1 :: Operand, 
      metadata :: InstructionMetadata
    }
  | Alloca { 
      allocatedType :: Type,
      numElements :: Maybe Operand,
      alignment :: Word32,
      metadata :: InstructionMetadata
    }
  | Load {
      volatile :: Bool, 
      address :: Operand,
      maybeAtomicity :: Maybe Atomicity,
      alignment :: Word32,
      metadata :: InstructionMetadata
    }
  | Store {
      volatile :: Bool, 
      address :: Operand,
      value :: Operand,
      maybeAtomicity :: Maybe Atomicity,
      alignment :: Word32,
      metadata :: InstructionMetadata
    }
  | GetElementPtr { 
      inBounds :: Bool,
      address :: Operand,
      indices :: [Operand],
      metadata :: InstructionMetadata
    }
  | Fence { 
      atomicity :: Atomicity,
      metadata :: InstructionMetadata 
    }
  | CmpXchg { 
      volatile :: Bool,
      address :: Operand,
      expected :: Operand,
      replacement :: Operand,
      atomicity :: Atomicity,
      metadata :: InstructionMetadata 
    }
  | AtomicRMW { 
      volatile :: Bool,
      rmwOperation :: RMWOperation,
      address :: Operand,
      value :: Operand,
      atomicity :: Atomicity,
      metadata :: InstructionMetadata 
    }
  | Trunc { 
      operand0 :: Operand,
      type' :: Type,
      metadata :: InstructionMetadata 
    }
  | ZExt {
      operand0 :: Operand,
      type' :: Type,
      metadata :: InstructionMetadata 
    }
  | SExt {
      operand0 :: Operand,
      type' :: Type,
      metadata :: InstructionMetadata
    }
  | FPToUI {
      operand0 :: Operand,
      type' :: Type,
      metadata :: InstructionMetadata
    }
  | FPToSI {
      operand0 :: Operand,
      type' :: Type,
      metadata :: InstructionMetadata
    }
  | UIToFP {
      operand0 :: Operand,
      type' :: Type,
      metadata :: InstructionMetadata
    }
  | SIToFP {
      operand0 :: Operand,
      type' :: Type,
      metadata :: InstructionMetadata
    }
  | FPTrunc {
      operand0 :: Operand,
      type' :: Type,
      metadata :: InstructionMetadata
    }
  | FPExt {
      operand0 :: Operand,
      type' :: Type,
      metadata :: InstructionMetadata
    }
  | PtrToInt {
      operand0 :: Operand,
      type' :: Type,
      metadata :: InstructionMetadata
    }
  | IntToPtr {
      operand0 :: Operand,
      type' :: Type,
      metadata :: InstructionMetadata
    }
  | BitCast {
      operand0 :: Operand,
      type' :: Type,
      metadata :: InstructionMetadata
    }
  | ICmp {
      iPredicate :: IntegerPredicate,
      operand0 :: Operand,
      operand1 :: Operand,
      metadata :: InstructionMetadata
    }
  | FCmp {
      fpPredicate :: FloatingPointPredicate,
      operand0 :: Operand,
      operand1 :: Operand,
      metadata :: InstructionMetadata
    }
  | Phi {
      type' :: Type,
      incomingValues :: [ (Operand, Name) ],
      metadata :: InstructionMetadata
  } 
  | Call {
      isTailCall :: Bool,
      callingConvention :: CallingConvention,
      returnAttributes :: [ParameterAttribute],
      function :: CallableOperand,
      arguments :: [(Operand, [ParameterAttribute])],
      functionAttributes :: [FunctionAttribute],
      metadata :: InstructionMetadata
  }
  | Select { 
      condition' :: Operand,
      trueValue :: Operand,
      falseValue :: Operand,
      metadata :: InstructionMetadata
    }
  | VAArg { 
      argList :: Operand,
      type' :: Type,
      metadata :: InstructionMetadata 
    }
  | ExtractElement { 
      vector :: Operand,
      index :: Operand,
      metadata :: InstructionMetadata 
    }
  | InsertElement { 
      vector :: Operand,
      element :: Operand,
      index :: Operand,
      metadata :: InstructionMetadata
    }
  | ShuffleVector { 
      operand0 :: Operand,
      operand1 :: Operand,
      mask :: Constant,
      metadata :: InstructionMetadata
    }
  | ExtractValue { 
      aggregate :: Operand,
      indices' :: [Word32],
      metadata :: InstructionMetadata
    }
  | InsertValue { 
      aggregate :: Operand,
      element :: Operand,
      indices' :: [Word32],
      metadata :: InstructionMetadata
    }
  | LandingPad { 
      type' :: Type,
      personalityFunction :: Operand,
      cleanup :: Bool,
      clauses :: [LandingPadClause],
      metadata :: InstructionMetadata 
    }
  deriving (Eq, Read, Show)

data Named a 
  = Name := a
  | Do a
  deriving (Eq, Read, Show)
