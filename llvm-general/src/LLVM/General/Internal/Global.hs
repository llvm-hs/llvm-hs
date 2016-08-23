{-# LANGUAGE
  TemplateHaskell,
  MultiParamTypeClasses
  #-}
module LLVM.General.Internal.Global where

import LLVM.General.Prelude

import Control.Monad.State
import Control.Monad.AnyCont
import Foreign.Ptr
import qualified Data.Map as Map

import qualified LLVM.General.Internal.FFI.PtrHierarchy as FFI
import qualified LLVM.General.Internal.FFI.LLVMCTypes as FFI
import qualified LLVM.General.Internal.FFI.GlobalValue as FFI

import LLVM.General.Internal.Coding
import LLVM.General.Internal.DecodeAST
import LLVM.General.Internal.EncodeAST

import qualified LLVM.General.AST.Linkage as A.L
import qualified LLVM.General.AST.Visibility as A.V
import qualified LLVM.General.AST.COMDAT as A.COMDAT
import qualified LLVM.General.AST.DLL as A.DLL
import qualified LLVM.General.AST.ThreadLocalStorage as A.TLS
import qualified LLVM.General.AST.Global as A.G

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
  (FFI.linkageExternalWeak, A.L.ExternWeak),
  (FFI.linkageCommon, A.L.Common)
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

genCodingInstance [t| Maybe A.DLL.StorageClass |] ''FFI.DLLStorageClass [
  (FFI.dllStorageClassDefault, Nothing),
  (FFI.dllStorageClassDLLImport, Just A.DLL.Import),
  (FFI.dllStorageClassDLLExport, Just A.DLL.Export)
 ]

getDLLStorageClass :: FFI.DescendentOf FFI.GlobalValue v => Ptr v -> DecodeAST (Maybe A.DLL.StorageClass)
getDLLStorageClass g = liftIO $ decodeM =<< FFI.getDLLStorageClass (FFI.upCast g)

setDLLStorageClass :: FFI.DescendentOf FFI.GlobalValue v => Ptr v -> Maybe A.DLL.StorageClass -> EncodeAST ()
setDLLStorageClass g sc = liftIO . FFI.setDLLStorageClass (FFI.upCast g) =<< encodeM sc

getSection :: FFI.DescendentOf FFI.GlobalValue v => Ptr v -> DecodeAST (Maybe String)
getSection g = liftIO $ do
  s <- decodeM =<< FFI.getSection (FFI.upCast g)
  return $ if (s == "") then Nothing else Just s

setSection :: FFI.DescendentOf FFI.GlobalValue v => Ptr v -> Maybe String -> EncodeAST ()
setSection g s = scopeAnyCont $ do
  s <- encodeM (maybe "" id s)
  liftIO $ FFI.setSection (FFI.upCast g) s

genCodingInstance [t| A.COMDAT.SelectionKind |] ''FFI.COMDATSelectionKind [
  (FFI.comdatSelectionKindAny, A.COMDAT.Any),
  (FFI.comdatSelectionKindExactMatch, A.COMDAT.ExactMatch),
  (FFI.comdatSelectionKindLargest, A.COMDAT.Largest),
  (FFI.comdatSelectionKindNoDuplicates, A.COMDAT.NoDuplicates),
  (FFI.comdatSelectionKindSameSize, A.COMDAT.SameSize)
 ]

instance DecodeM DecodeAST (String, A.COMDAT.SelectionKind) (Ptr FFI.COMDAT) where
  decodeM c = return (,)
              `ap` (decodeM $ FFI.getCOMDATName c)
              `ap` (decodeM =<< liftIO (FFI.getCOMDATSelectionKind c))

getCOMDATName :: FFI.DescendentOf FFI.GlobalValue v => Ptr v -> DecodeAST (Maybe String)
getCOMDATName g = do
  c <- liftIO $ FFI.getCOMDAT (FFI.upCast g)
  if c == nullPtr
   then return Nothing
   else do
     cds <- gets comdats
     liftM Just $ case Map.lookup c cds of
       Just (name, _) -> return name
       Nothing -> do
          cd@(name, _) <- decodeM c
          modify $ \s -> s { comdats = Map.insert c cd cds }
          return name

setCOMDAT :: FFI.DescendentOf FFI.GlobalObject v => Ptr v -> Maybe String -> EncodeAST ()
setCOMDAT _ Nothing = return ()
setCOMDAT g (Just name) = do
  cd <- referCOMDAT name
  liftIO $ FFI.setCOMDAT (FFI.upCast g) cd

setAlignment :: FFI.DescendentOf FFI.GlobalValue v => Ptr v -> Word32 -> EncodeAST ()
setAlignment g i = liftIO $ FFI.setAlignment (FFI.upCast g) (fromIntegral i)

getAlignment :: FFI.DescendentOf FFI.GlobalValue v => Ptr v -> DecodeAST Word32
getAlignment g = liftIO $ fromIntegral <$> FFI.getAlignment (FFI.upCast g)

genCodingInstance [t| Maybe A.TLS.Model |] ''FFI.ThreadLocalMode [
  (FFI.threadLocalModeNotThreadLocal, Nothing),
  (FFI.threadLocalModeGeneralDynamicTLSModel, Just A.TLS.GeneralDynamic),
  (FFI.threadLocalModeLocalDynamicTLSModel, Just A.TLS.LocalDynamic),
  (FFI.threadLocalModeInitialExecTLSModel, Just A.TLS.InitialExec),
  (FFI.threadLocalModeLocalExecTLSModel, Just A.TLS.LocalExec)
 ]

getThreadLocalMode :: FFI.DescendentOf FFI.GlobalValue v => Ptr v -> DecodeAST (Maybe A.TLS.Model)
getThreadLocalMode g = liftIO $ decodeM =<< FFI.getThreadLocalMode (FFI.upCast g)

setThreadLocalMode :: FFI.DescendentOf FFI.GlobalValue v => Ptr v -> Maybe A.TLS.Model -> EncodeAST ()
setThreadLocalMode g m = liftIO . FFI.setThreadLocalMode (FFI.upCast g) =<< encodeM m

genCodingInstance [t| Maybe A.G.UnnamedAddr |] ''FFI.UnnamedAddr [
  (FFI.unnamedAddrNone, Nothing),
  (FFI.unnamedAddrLocal, Just A.G.LocalAddr),
  (FFI.unnamedAddrGlobal, Just A.G.GlobalAddr)
 ]
