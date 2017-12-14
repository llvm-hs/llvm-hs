{-# LANGUAGE
  ForeignFunctionInterface,
  MultiParamTypeClasses,
  FunctionalDependencies,
  UndecidableInstances
  #-}
-- | This module defines typeclasses to represent the relationships of an object-oriented inheritance hierarchy
module LLVM.Internal.FFI.PtrHierarchy where

import LLVM.Prelude

import Foreign.Ptr

-- | a class to represent safe casting of pointers to objects of descendant-classes to ancestor-classes.
class DescendentOf a b where
    upCast :: Ptr b -> Ptr a
    upCast = castPtr

-- | trivial casts
instance {-# OVERLAPPING #-} DescendentOf a a where
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

-- | <http://llvm.org/doxygen/classllvm_1_1GlobalObject.html>
data GlobalObject

instance ChildOf GlobalValue GlobalObject

-- | <http://llvm.org/doxygen/classllvm_1_1GlobalVariable.html>
data GlobalVariable

instance ChildOf GlobalObject GlobalVariable

-- | <http://llvm.org/doxygen/classllvm_1_1GlobalAlias.html>
data GlobalAlias

instance ChildOf GlobalValue GlobalAlias

-- | <http://llvm.org/doxygen/classllvm_1_1Function.html>
data Function

instance ChildOf GlobalObject Function

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

instance ChildOf Metadata MDNode

data DIFile

instance ChildOf MDNode DIFile

-- | <http://llvm.org/doxygen/classllvm_1_1MDString.html>
data MDString

instance ChildOf Metadata MDString

-- | http://llvm.org/doxygen/classllvm_1_1ValueAsMetadata.html
data MDValue

instance ChildOf Metadata MDValue

-- | <http://llvm.org/doxygen/classllvm_1_1NamedMDNode.html>
data NamedMetadata

-- | <http://llvm.org/doxygen/classllvm_1_1InlineAsm.html>
data InlineAsm

instance ChildOf Value InlineAsm

-- | <http://llvm.org/doxygen/classllvm_1_1Type.html>
data Type

-- | <http://llvm.org/doxygen/classllvm_1_1Metadata.html>
data Metadata

-- | <http://www.llvm.org/docs/doxygen/html/classllvm_1_1MetadataAsValue.html>
data MetadataAsVal

instance ChildOf Value MetadataAsVal

-- | <http://llvm.org/docs/doxygen/html/classllvm_1_1raw__ostream.html>
data RawOStream

-- | <http://llvm.org/docs/doxygen/html/classllvm_1_1raw__pwrite__stream.html>
data RawPWriteStream

instance ChildOf RawOStream RawPWriteStream
