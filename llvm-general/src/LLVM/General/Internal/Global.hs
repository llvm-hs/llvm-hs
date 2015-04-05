{-# LANGUAGE
  TemplateHaskell,
  MultiParamTypeClasses
  #-}
module LLVM.General.Internal.Global where

import LLVM.General.Prelude

import Control.Monad.IO.Class
import Foreign.Ptr
import Control.Monad.AnyCont

import qualified LLVM.General.Internal.FFI.PtrHierarchy as FFI
import qualified LLVM.General.Internal.FFI.LLVMCTypes as FFI
import qualified LLVM.General.Internal.FFI.GlobalValue as FFI

import LLVM.General.Internal.Coding
import LLVM.General.Internal.DecodeAST
import LLVM.General.Internal.EncodeAST

import qualified LLVM.General.AST.Linkage as A.L
import qualified LLVM.General.AST.Visibility as A.V

genCodingInstance [t| A.L.Linkage |] ''FFI.Linkage [
  (FFI.linkageExternal, A.L.External),
  (FFI.linkageAvailableExternally, A.L.AvailableExternally),
  (FFI.linkageLinkOnceAny, A.L.LinkOnce),
  (FFI.linkageLinkOnceODR, A.L.LinkOnceODR),
  (FFI.linkageWeakAny, A.L.Weak),
  (FFI.linkageWeakODR, A.L.WeakODR),
  (FFI.linkageAppending, A.L.Appending),
  (FFI.linkageInternal, A.L.Internal),
  (FFI.linkagePrivate, A.L.Private),
  (FFI.linkageDLLImport, A.L.DLLImport),
  (FFI.linkageDLLExport, A.L.DLLExport),
  (FFI.linkageExternalWeak, A.L.ExternWeak),
  (FFI.linkageCommon, A.L.Common),
  (FFI.linkageLinkerPrivate, A.L.LinkerPrivate),
  (FFI.linkageLinkerPrivateWeak, A.L.LinkerPrivateWeak)
 ]

getLinkage :: FFI.DescendentOf FFI.GlobalValue v => Ptr v -> DecodeAST A.L.Linkage
getLinkage g = liftIO $ decodeM =<< FFI.getLinkage (FFI.upCast g)

setLinkage :: FFI.DescendentOf FFI.GlobalValue v => Ptr v -> A.L.Linkage -> EncodeAST ()
setLinkage g l = liftIO . FFI.setLinkage (FFI.upCast g) =<< encodeM l
                                                                       
genCodingInstance [t| A.V.Visibility |] ''FFI.Visibility [
  (FFI.visibilityDefault, A.V.Default),
  (FFI.visibilityHidden, A.V.Hidden),
  (FFI.visibilityProtected, A.V.Protected)
 ]

getVisibility :: FFI.DescendentOf FFI.GlobalValue v => Ptr v -> DecodeAST A.V.Visibility
getVisibility g = liftIO $ decodeM =<< FFI.getVisibility (FFI.upCast g)

setVisibility :: FFI.DescendentOf FFI.GlobalValue v => Ptr v -> A.V.Visibility -> EncodeAST ()
setVisibility g v = liftIO . FFI.setVisibility (FFI.upCast g) =<< encodeM v

getSection :: FFI.DescendentOf FFI.GlobalValue v => Ptr v -> DecodeAST (Maybe String)
getSection g = liftIO $ do
  s <- decodeM =<< FFI.getSection (FFI.upCast g)
  return $ if (s == "") then Nothing else Just s

setSection :: FFI.DescendentOf FFI.GlobalValue v => Ptr v -> Maybe String -> EncodeAST ()
setSection g s = scopeAnyCont $ do
  s <- encodeM (maybe "" id s)
  liftIO $ FFI.setSection (FFI.upCast g) s

setAlignment :: FFI.DescendentOf FFI.GlobalValue v => Ptr v -> Word32 -> EncodeAST ()
setAlignment g i = liftIO $ FFI.setAlignment (FFI.upCast g) (fromIntegral i)

getAlignment :: FFI.DescendentOf FFI.GlobalValue v => Ptr v -> DecodeAST Word32
getAlignment g = liftIO $ fromIntegral <$> FFI.getAlignment (FFI.upCast g)
