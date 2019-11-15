-- | Module to allow importing 'Attribute' distinctly qualified.
-- Before LLVM 3.5, the attributes which could be used on functions
-- and those which could be used on parameters were disjoint.  In
-- LLVM 3.5, two attributes (readonly and readnone) can be used
-- in both contexts. Because their interpretation is different in
-- the two contexts and only those two attributes can be used in
-- both contexts, I've opted to keep the Haskell types for parameter
-- and function attributes distinct, but move the two types into
-- separate modules so they can have contructors with the same names.
module LLVM.AST.Attribute (
    ParameterAttribute(..),
    FunctionAttribute(..),
    GroupID(..)
  ) where

import LLVM.AST.ParameterAttribute
       hiding (NoFree, ReadNone, ReadOnly, StringAttribute, WriteOnly,
               stringAttributeKind, stringAttributeValue)
import LLVM.AST.FunctionAttribute
