{-# LANGUAGE
  MultiParamTypeClasses
  #-}
module LLVM.Internal.Value where

import LLVM.Prelude

import Control.Monad.State

import Foreign.Ptr

import qualified LLVM.Internal.FFI.PtrHierarchy as FFI
import qualified LLVM.Internal.FFI.Value as FFI

import LLVM.Internal.Coding
import LLVM.Internal.DecodeAST
import LLVM.Internal.Type () 

import qualified LLVM.AST.Type as A

typeOf :: FFI.DescendentOf FFI.Value v => Ptr v -> DecodeAST A.Type
typeOf = decodeM <=< liftIO . FFI.typeOf . FFI.upCast

