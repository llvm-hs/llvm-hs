{-# LANGUAGE
  MultiParamTypeClasses
  #-}
module LLVM.General.Internal.Value where

import LLVM.General.Prelude

import Control.Monad.State

import Foreign.Ptr

import qualified LLVM.General.Internal.FFI.PtrHierarchy as FFI
import qualified LLVM.General.Internal.FFI.Value as FFI

import LLVM.General.Internal.Coding
import LLVM.General.Internal.DecodeAST
import LLVM.General.Internal.Type () 
import LLVM.General.Internal.Constant () 

import qualified LLVM.General.AST.Type as A

typeOf :: FFI.DescendentOf FFI.Value v => Ptr v -> DecodeAST A.Type
typeOf = decodeM <=< liftIO . FFI.typeOf . FFI.upCast

