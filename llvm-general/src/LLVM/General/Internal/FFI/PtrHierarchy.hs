{-# LANGUAGE
  ForeignFunctionInterface,
  MultiParamTypeClasses,
  FunctionalDependencies,
  UndecidableInstances,
  CPP
  #-}
#if __GLASGOW_HASKELL__ < 710
{-# LANGUAGE OverlappingInstances #-}
#define CPP_OVERLAPPING
#else
#define CPP_OVERLAPPING {-# OVERLAPPING #-}
#endif
-- | This module defines typeclasses to represent the relationships of an object-oriented inheritance hierarchy
module LLVM.General.Internal.FFI.PtrHierarchy where

import Foreign.Ptr

-- | a class to represent safe casting of pointers to objects of descendant-classes to ancestor-classes.
class DescendentOf a b where
    upCast :: Ptr b -> Ptr a
    upCast = castPtr

-- | trivial casts
instance CPP_OVERLAPPING DescendentOf a a where
    upCast = id

-- | a class to represent direct parent-child relationships
class ChildOf b c | c -> b

-- | ancestor-descentant relationships are build out of parent-child relationships
instance (DescendentOf a b, ChildOf b c) => DescendentOf a c

-- | <http://llvm.org/doxygen/classllvm_1_1Value.html>
data Value

-- | <http://llvm.org/doxygen/classllvm_1_1Constant.html>
data Constant

instance ChildOf User Constant

-- | <http://llvm.org/doxygen/classllvm_1_1GlobalValue.html>
data GlobalValue

instance ChildOf Constant GlobalValue

-- | <http://llvm.org/doxygen/classllvm_1_1GlobalVariable.html>
data GlobalVariable

instance ChildOf GlobalValue GlobalVariable

-- | <http://llvm.org/doxygen/classllvm_1_1GlobalAlias.html>
data GlobalAlias

instance ChildOf GlobalValue GlobalAlias

-- | <http://llvm.org/doxygen/classllvm_1_1Function.html>
data Function

instance ChildOf GlobalValue Function

-- | <http://llvm.org/doxygen/classllvm_1_1BasicBlock.html>
data BasicBlock

instance ChildOf Value BasicBlock

-- | <http://llvm.org/doxygen/classllvm_1_1Argument.html>
data Parameter

instance ChildOf Value Parameter

-- | <http://llvm.org/doxygen/classllvm_1_1Instruction.html>
data Instruction

instance ChildOf User Instruction

-- | <http://llvm.org/doxygen/classllvm_1_1BinaryOperator.html>
data BinaryOperator

instance ChildOf Instruction BinaryOperator

-- | <http://llvm.org/doxygen/classllvm_1_1User.html>
data User

instance ChildOf Value User

-- | <http://llvm.org/doxygen/classllvm_1_1MDNode.html>
data MDNode

instance ChildOf Value MDNode

-- | <http://llvm.org/doxygen/classllvm_1_1MDString.html>
data MDString

instance ChildOf Value MDString

-- | <http://llvm.org/doxygen/classllvm_1_1NamedMDNode.html>
data NamedMetadata

-- | <http://llvm.org/doxygen/classllvm_1_1InlineAsm.html>
data InlineAsm

instance ChildOf Value InlineAsm

-- | <http://llvm.org/doxygen/classllvm_1_1Type.html>
data Type

