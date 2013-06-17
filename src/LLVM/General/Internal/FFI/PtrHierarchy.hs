{-# LANGUAGE
  ForeignFunctionInterface,
  MultiParamTypeClasses,
  EmptyDataDecls,
  FunctionalDependencies,
  FlexibleInstances,
  UndecidableInstances,
  OverlappingInstances
  #-}

-- | This module defines typeclasses to represent the relationships of an object-oriented inheritance hierarchy
module LLVM.General.Internal.FFI.PtrHierarchy where

import Foreign.Ptr

-- | a class to represent safe casting of pointers to objects of descendant-classes to ancestor-classes.
class DescendentOf a b where
    upCast :: Ptr b -> Ptr a
    upCast = castPtr

-- | trivial casts
instance DescendentOf a a where
    upCast = id

-- | a class to represent direct parent-child relationships
class ChildOf b c | c -> b

-- | ancestor-descentant relationships are build out of parent-child relationships
instance (DescendentOf a b, ChildOf b c) => DescendentOf a c

-- | a blind type to correspond to llvm::Value
data Value

-- | a blind type to correspond to llvm::Constant
data Constant

instance ChildOf User Constant

data GlobalValue

instance ChildOf Constant GlobalValue

-- | a blind type to correspond to llvm::GlobalVariable
data GlobalVariable

instance ChildOf GlobalValue GlobalVariable

-- | a blind type to correspond to llvm::GlobalAlias
data GlobalAlias

instance ChildOf GlobalValue GlobalAlias

data Function

instance ChildOf GlobalValue Function

data BasicBlock

instance ChildOf Value BasicBlock

data Parameter

instance ChildOf Value Parameter

data Instruction

instance ChildOf User Instruction

-- | a blind type to correspond to llvm::User
data User

instance ChildOf Value User

data MDNode

instance ChildOf Value MDNode

data MDString

instance ChildOf Value MDString

data NamedMetadata

data InlineAsm

instance ChildOf Value InlineAsm
