-- | LLVM instructions 
-- <http://llvm.org/docs/LangRef.html#instruction-reference>
module LLVM.General.AST.Instruction where

import LLVM.General.Prelude

import LLVM.General.AST.Type
import LLVM.General.AST.Name
import LLVM.General.AST.Constant
import LLVM.General.AST.Operand
import LLVM.General.AST.IntegerPredicate (IntegerPredicate)
import LLVM.General.AST.FloatingPointPredicate (FloatingPointPredicate)
import LLVM.General.AST.RMWOperation (RMWOperation)
import LLVM.General.AST.CallingConvention (CallingConvention)
import LLVM.General.AST.Attribute (ParameterAttribute, FunctionAttribute)

-- | <http://llvm.org/docs/LangRef.html#metadata-nodes-and-metadata-strings>
-- Metadata can be attached to an instruction
type InstructionMetadata = [(String, MetadataNode)]

-- | <http://llvm.org/docs/LangRef.html#terminators>
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
  deriving (Eq, Read, Show, Typeable, Data)

-- | <http://llvm.org/docs/LangRef.html#fast-math-flags>
data FastMathFlags 
  = NoFastMathFlags
  | UnsafeAlgebra
  | FastMathFlags {
      noNaNs :: Bool,
      noInfs :: Bool,
      noSignedZeros :: Bool,
      allowReciprocal :: Bool
    }
  deriving (Eq, Ord, Read, Show, Data, Typeable)

-- | <http://llvm.org/docs/LangRef.html#atomic-memory-ordering-constraints>
-- <http://llvm.org/docs/Atomics.html>
data MemoryOrdering
  = Unordered
  | Monotonic
  | Acquire
  | Release
  | AcquireRelease
  | SequentiallyConsistent
  deriving (Eq, Ord, Read, Show, Data, Typeable)

-- | <http://llvm.org/docs/LangRef.html#singlethread>
data SynchronizationScope
  = SingleThread
  | CrossThread
  deriving (Eq, Ord, Read, Show, Data, Typeable)

-- | An 'Atomicity' describes constraints on the visibility of effects of an atomic instruction
type Atomicity = (SynchronizationScope, MemoryOrdering)

-- | For the redoubtably complex 'LandingPad' instruction
data LandingPadClause
    = Catch Constant
    | Filter Constant
    deriving (Eq, Ord, Read, Show, Typeable, Data)

-- | non-terminator instructions:
-- <http://llvm.org/docs/LangRef.html#binaryops>
-- <http://llvm.org/docs/LangRef.html#bitwiseops>
-- <http://llvm.org/docs/LangRef.html#memoryops>
-- <http://llvm.org/docs/LangRef.html#otherops>
data Instruction
  = Add { 
      nsw :: Bool,
      nuw :: Bool,
      operand0 :: Operand,
      operand1 :: Operand,
      metadata :: InstructionMetadata
    }
  | FAdd {
      fastMathFlags :: FastMathFlags,
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
      fastMathFlags :: FastMathFlags,
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
      fastMathFlags :: FastMathFlags,
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
      fastMathFlags :: FastMathFlags,
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
      fastMathFlags :: FastMathFlags,
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
      failureMemoryOrdering :: MemoryOrdering,
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
  | AddrSpaceCast {
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
  deriving (Eq, Read, Show, Typeable, Data)

-- | Instances of instructions may be given a name, allowing their results to be referenced as 'Operand's.
-- Sometimes instructions - e.g. a call to a function returning void - don't need names.
data Named a 
  = Name := a
  | Do a
  deriving (Eq, Read, Show, Typeable, Data)
