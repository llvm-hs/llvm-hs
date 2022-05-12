{-# LANGUAGE
  TemplateHaskell,
  MultiParamTypeClasses,
  OverloadedStrings
  #-}
module LLVM.Internal.InlineAssembly where

import LLVM.Prelude

import Control.Monad.IO.Class

import qualified Data.ByteString.Char8 as ByteString

import Foreign.C
import Foreign.Ptr

import qualified LLVM.Internal.FFI.InlineAssembly as FFI
import qualified LLVM.Internal.FFI.LLVMCTypes as FFI
import qualified LLVM.Internal.FFI.Module as FFI
import qualified LLVM.Internal.FFI.PtrHierarchy as FFI

import qualified LLVM.AST as A (Definition(..))
import qualified LLVM.AST.InlineAssembly as A

import LLVM.Internal.Coding
import LLVM.Internal.EncodeAST
import LLVM.Internal.DecodeAST
import LLVM.Internal.Type ()

genCodingInstance [t| A.Dialect |] ''FFI.AsmDialect [
   (FFI.asmDialectATT, A.ATTDialect),
   (FFI.asmDialectIntel, A.IntelDialect)
  ]

instance EncodeM EncodeAST A.InlineAssembly (Ptr FFI.InlineAsm) where
  encodeM (A.InlineAssembly {
             A.type' = t,
             A.assembly = assembly,
             A.constraints = constraints,
             A.hasSideEffects = hasSideEffects,
             A.alignStack = alignStack,
             A.dialect = dialect
           }) = do
    t <- encodeM t
    assembly <- encodeM assembly
    constraints <- encodeM constraints
    hasSideEffects <- encodeM hasSideEffects
    alignStack <- encodeM alignStack
    dialect <- encodeM dialect
    liftIO $ FFI.createInlineAsm t assembly constraints hasSideEffects alignStack dialect

instance DecodeM DecodeAST A.InlineAssembly (Ptr FFI.InlineAsm) where
  decodeM p = do
    return A.InlineAssembly
      `ap` (decodeM =<< liftIO (FFI.getInlineAsmFunctionType p))
      `ap` (decodeM =<< liftIO (FFI.getInlineAsmAssemblyString p))
      `ap` (decodeM =<< liftIO (FFI.getInlineAsmConstraintString p))
      `ap` (decodeM =<< liftIO (FFI.inlineAsmHasSideEffects p))
      `ap` (decodeM =<< liftIO (FFI.inlineAsmIsAlignStack p))
      `ap` (decodeM =<< liftIO (FFI.getInlineAsmDialect p))

instance DecodeM DecodeAST [A.Definition] (FFI.ModuleAsm CString) where
  decodeM (FFI.ModuleAsm s) = do
    s' <- decodeM s
    return . map A.ModuleInlineAssembly $ ByteString.lines s'
