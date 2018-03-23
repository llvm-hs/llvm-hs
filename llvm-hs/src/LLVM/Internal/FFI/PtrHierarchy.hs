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

data MDTuple

instance ChildOf MDNode MDTuple

-- | <http://llvm.org/doxygen/classllvm_1_1MDString.html>
data MDString

instance ChildOf Metadata MDString

-- | http://llvm.org/doxygen/classllvm_1_1ValueAsMetadata.html
data MDValue

instance ChildOf Metadata MDValue

-- | https://llvm.org/doxygen/classllvm_1_1DIExpression.html
data DIExpression

instance ChildOf MDNode DIExpression

-- | https://llvm.org/doxygen/classllvm_1_1DIGlobalVariableExpression.html
data DIGlobalVariableExpression

instance ChildOf MDNode DIGlobalVariableExpression

-- | https://llvm.org/doxygen/classllvm_1_1DILocation.html
data DILocation

instance ChildOf MDNode DILocation

-- | https://llvm.org/doxygen/classllvm_1_1DINode.html
data DINode

instance ChildOf MDNode DINode

-- | https://llvm.org/doxygen/classllvm_1_1DIImportedEntity.html
data DIImportedEntity

instance ChildOf DINode DIImportedEntity

-- | https://llvm.org/doxygen/classllvm_1_1DIObjCProperty.html
data DIObjCProperty

instance ChildOf DINode DIObjCProperty

-- | https://llvm.org/doxygen/classllvm_1_1DISubrange.html
data DISubrange

instance ChildOf DINode DISubrange

-- | https://llvm.org/doxygen/classllvm_1_1DIEnumerator.html
data DIEnumerator

instance ChildOf DINode DIEnumerator

-- | https://llvm.org/doxygen/classllvm_1_1DIVariable.html
data DIVariable

instance ChildOf DINode DIVariable

-- | https://llvm.org/doxygen/classllvm_1_1DILocalVariable.html
data DILocalVariable

instance ChildOf DIVariable DILocalVariable

-- | https://llvm.org/doxygen/classllvm_1_1DIGlobalVariable.html
data DIGlobalVariable

instance ChildOf DIVariable DIGlobalVariable

-- | https://llvm.org/doxygen/classllvm_1_1DITemplateParameter.html
data DITemplateParameter

instance ChildOf DINode DITemplateParameter

-- | https://llvm.org/doxygen/classllvm_1_1DITemplateTypeParameter.html
data DITemplateTypeParameter

instance ChildOf DITemplateParameter DITemplateTypeParameter

-- | https://llvm.org/doxygen/classllvm_1_1DITemplateValueParameter.html
data DITemplateValueParameter

instance ChildOf DITemplateParameter DITemplateValueParameter

-- | https://llvm.org/doxygen/classllvm_1_1DIScope.html
data DIScope

instance ChildOf DINode DIScope

-- | https://llvm.org/doxygen/classllvm_1_1DIModule.html
data DIModule

instance ChildOf DIScope DIModule

-- | https://llvm.org/doxygen/classllvm_1_1DINamespace.html
data DINamespace

instance ChildOf DIScope DINamespace

-- | https://llvm.org/doxygen/classllvm_1_1DIFile.html
data DIFile

instance ChildOf DIScope DIFile

-- | https://llvm.org/doxygen/classllvm_1_1DICompileUnit.html
data DICompileUnit

instance ChildOf DIScope DICompileUnit

-- | https://llvm.org/doxygen/classllvm_1_1DIType.html
data DIType

instance ChildOf DIScope DIType

-- | https://llvm.org/doxygen/classllvm_1_1DIBasicType.html
data DIBasicType

instance ChildOf DIType DIBasicType

-- | https://llvm.org/doxygen/classllvm_1_1DIDerivedType.html
data DIDerivedType

instance ChildOf DIType DIDerivedType

-- | https://llvm.org/doxygen/classllvm_1_1DISubroutineType.html
data DISubroutineType

instance ChildOf DIType DISubroutineType

-- | https://llvm.org/doxygen/classllvm_1_1DICompositeType.html
data DICompositeType

instance ChildOf DIType DICompositeType

-- | https://llvm.org/doxygen/classllvm_1_1DILocalScope.html
data DILocalScope

instance ChildOf DIScope DILocalScope

-- | <https://llvm.org/doxygen/classllvm_1_1DILexicalBlockBase.html>
data DILexicalBlockBase

instance ChildOf DILocalScope DILexicalBlockBase

-- | <https://llvm.org/doxygen/classllvm_1_1DILexicalBlock.html>
data DILexicalBlock

instance ChildOf DILexicalBlockBase DILexicalBlock

-- | <https://llvm.org/doxygen/classllvm_1_1DILexicalBlockFile.html>
data DILexicalBlockFile

instance ChildOf DILexicalBlockBase DILexicalBlockFile

-- | <https://llvm.org/doxygen/classllvm_1_1DISubprogram.html>
data DISubprogram

instance ChildOf DILocalScope DISubprogram

-- <https://llvm.org/doxygen/classllvm_1_1DIMacroNode.html>
data DIMacroNode

instance ChildOf MDNode DIMacroNode

-- <https://llvm.org/doxygen/classllvm_1_1DIMacro.html>
data DIMacro

instance ChildOf DIMacroNode DIMacro

-- <https://llvm.org/doxygen/classllvm_1_1DIMacroFile.html>
data DIMacroFile

instance ChildOf DIMacroNode DIMacroFile

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

-- | <https://llvm.org/doxygen/classllvm_1_1StringRef.html>
data StringRef
