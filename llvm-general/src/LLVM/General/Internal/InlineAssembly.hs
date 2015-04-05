{-# LANGUAGE
  TemplateHaskell,
  MultiParamTypeClasses
  #-}
module LLVM.General.Internal.InlineAssembly where
 
import LLVM.General.Prelude

import Control.Monad.IO.Class

import Foreign.C
import Foreign.Ptr

import qualified LLVM.General.Internal.FFI.InlineAssembly as FFI
import qualified LLVM.General.Internal.FFI.LLVMCTypes as FFI
import qualified LLVM.General.Internal.FFI.Module as FFI
import qualified LLVM.General.Internal.FFI.PtrHierarchy as FFI

import qualified LLVM.General.AST as A (Definition(..))
import qualified LLVM.General.AST.InlineAssembly as A
import qualified LLVM.General.AST.Type as A

import LLVM.General.Internal.Coding 
import LLVM.General.Internal.EncodeAST
import LLVM.General.Internal.DecodeAST
import LLVM.General.Internal.Value

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
      `ap` (liftM (\(A.PointerType f _) -> f) (typeOf p))
      `ap` (decodeM =<< liftIO (FFI.getInlineAsmAssemblyString p))
      `ap` (decodeM =<< liftIO (FFI.getInlineAsmConstraintString p))
      `ap` (decodeM =<< liftIO (FFI.inlineAsmHasSideEffects p))
      `ap` (decodeM =<< liftIO (FFI.inlineAsmIsAlignStack p))
      `ap` (decodeM =<< liftIO (FFI.getInlineAsmDialect p))

instance DecodeM DecodeAST [A.Definition] (FFI.ModuleAsm CString) where
  decodeM (FFI.ModuleAsm s) = do
    s <- decodeM s
    let takeModIA "" = []
        takeModIA s =
          let (a,r) = break (== '\n') s
          in A.ModuleInlineAssembly a : takeModIA (dropWhile (== '\n') r)
    return $ takeModIA s
    
