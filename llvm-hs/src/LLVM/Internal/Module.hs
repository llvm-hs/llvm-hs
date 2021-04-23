{-#
  LANGUAGE
  TemplateHaskell,
  ScopedTypeVariables,
  MultiParamTypeClasses
  #-}
-- | This Haskell module is for/of functions for handling LLVM modules.
module LLVM.Internal.Module where

import LLVM.Prelude

import Control.Monad.AnyCont
import Control.Monad.Catch
import Control.Monad.Trans.Except
import Control.Monad.State (gets)
import Control.Monad.Trans

import Foreign.Ptr
import Foreign.C
import Data.IORef
import qualified Data.ByteString as BS
import qualified Data.ByteString.Short as SBS
import qualified Data.Map as Map

import qualified LLVM.Internal.FFI.Assembly as FFI
import qualified LLVM.Internal.FFI.Attribute as FFI
import qualified LLVM.Internal.FFI.Builder as FFI
import qualified LLVM.Internal.FFI.Bitcode as FFI
import qualified LLVM.Internal.FFI.Function as FFI
import qualified LLVM.Internal.FFI.GlobalAlias as FFI
import qualified LLVM.Internal.FFI.GlobalValue as FFI
import qualified LLVM.Internal.FFI.GlobalVariable as FFI
import qualified LLVM.Internal.FFI.Iterate as FFI
import qualified LLVM.Internal.FFI.LLVMCTypes as FFI
import qualified LLVM.Internal.FFI.MemoryBuffer as FFI
import qualified LLVM.Internal.FFI.Metadata as FFI
import qualified LLVM.Internal.FFI.Module as FFI
import qualified LLVM.Internal.FFI.PtrHierarchy as FFI
import qualified LLVM.Internal.FFI.RawOStream as FFI
import qualified LLVM.Internal.FFI.Target as FFI
import qualified LLVM.Internal.FFI.Value as FFI

import LLVM.Internal.Attribute
import LLVM.Internal.BasicBlock
import LLVM.Internal.Coding
import LLVM.Internal.Context
import LLVM.Internal.DecodeAST
import LLVM.Internal.EncodeAST
import LLVM.Internal.Function
import LLVM.Internal.Global
import LLVM.Internal.Instruction ()
import qualified LLVM.Internal.MemoryBuffer as MB
import LLVM.Internal.Metadata
import LLVM.Internal.Operand
import LLVM.Internal.RawOStream
import LLVM.Internal.String
import LLVM.Internal.Target
import LLVM.Internal.Type
import LLVM.Internal.Value

import LLVM.DataLayout
import LLVM.Exception

import qualified LLVM.AST as A
import qualified LLVM.AST.DataLayout as A
import qualified LLVM.AST.AddrSpace as A
import qualified LLVM.AST.Global as A.G

-- | <http://llvm.org/doxygen/classllvm_1_1Module.html>
newtype Module = Module (IORef (Ptr FFI.Module))

newModule :: Ptr FFI.Module -> IO (Module)
newModule m = fmap Module (newIORef m)

readModule :: MonadIO m => Module -> m (Ptr FFI.Module)
readModule (Module ref) = liftIO $ readIORef ref

-- | Signal that a module does no longer exist and thus must not be
-- disposed. It is the responsibility of the caller to ensure that the
-- module has been disposed. If you use only the functions provided by
-- llvm-hs you should never call this yourself.
deleteModule :: Module -> IO ()
deleteModule (Module r) = writeIORef r nullPtr

-- | A newtype to distinguish strings used for paths from other strings
newtype File = File FilePath
  deriving (Eq, Ord, Read, Show)

-- | link LLVM modules - move or copy parts of a source module into a
-- destination module.  Note that this operation is not commutative -
-- not only concretely (e.g. the destination module is modified,
-- becoming the result) but abstractly (e.g. unused private globals in
-- the source module do not appear in the result, but similar globals
-- in the destination remain). The source module is destroyed. May
-- throw a 'LinkException'.
linkModules ::
     Module -- ^ The module into which to link
  -> Module -- ^ The module to link into the other (this module is destroyed)
  -> IO ()
linkModules dest src  = flip runAnyContT return $ do
  dest' <- readModule dest
  src' <- readModule src
  result <- decodeM =<< liftIO (FFI.linkModules dest' src')
  -- linkModules takes care of deleting the sourcemodule
  liftIO $ deleteModule src
  when result (throwM $ LinkException "Couldn’t link modules")

class LLVMAssemblyInput s where
  llvmAssemblyMemoryBuffer :: (MonadThrow m, MonadIO m, MonadAnyCont IO m)
                              => s -> m (FFI.OwnerTransfered (Ptr FFI.MemoryBuffer))

instance LLVMAssemblyInput (String, String) where
  llvmAssemblyMemoryBuffer (id, s) = do
    UTF8ByteString bs <- encodeM s
    encodeM (MB.Bytes id bs)

instance LLVMAssemblyInput (String, ByteString) where
  llvmAssemblyMemoryBuffer (id, s) = do
    encodeM (MB.Bytes id s)

instance LLVMAssemblyInput String where
  llvmAssemblyMemoryBuffer s = llvmAssemblyMemoryBuffer ("<string>", s)

instance LLVMAssemblyInput ByteString where
  llvmAssemblyMemoryBuffer s = llvmAssemblyMemoryBuffer ("<string>", s)

instance LLVMAssemblyInput File where
  llvmAssemblyMemoryBuffer (File p) = encodeM (MB.File p)

-- | parse 'Module' from LLVM assembly. May throw 'ParseFailureException'.
withModuleFromLLVMAssembly :: LLVMAssemblyInput s
                              => Context -> s -> (Module -> IO a) -> IO a
withModuleFromLLVMAssembly (Context c) s f = flip runAnyContT return $ do
  mb <- llvmAssemblyMemoryBuffer s
  msgPtr <- alloca
  m <- anyContToM $ bracket (newModule =<< FFI.parseLLVMAssembly c mb msgPtr) (FFI.disposeModule <=< readModule)
  m' <- readModule m
  when (m' == nullPtr) $ throwM . ParseFailureException =<< decodeM msgPtr
  liftIO $ f m

-- | generate LLVM assembly from a 'Module'
moduleLLVMAssembly :: Module -> IO ByteString
moduleLLVMAssembly m = do
  resultRef <- newIORef Nothing
  let saveBuffer :: Ptr CChar -> CSize -> IO ()
      saveBuffer start size = do
        r <- decodeM (start, size)
        writeIORef resultRef (Just r)
  m' <- readModule m
  FFI.withBufferRawPWriteStream saveBuffer $ FFI.writeLLVMAssembly m' . FFI.upCast
  Just s <- readIORef resultRef
  return s

-- | write LLVM assembly for a 'Module' to a file
writeLLVMAssemblyToFile :: File -> Module -> IO ()
writeLLVMAssemblyToFile (File path) m = flip runAnyContT return $ do
  m' <- readModule m
  withFileRawOStream path False True $ FFI.writeLLVMAssembly m'

class BitcodeInput b where
  bitcodeMemoryBuffer :: (MonadThrow m, MonadIO m, MonadAnyCont IO m)
                         => b -> m (Ptr FFI.MemoryBuffer)

instance BitcodeInput (String, BS.ByteString) where
  bitcodeMemoryBuffer (s, bs) = encodeM (MB.Bytes s bs)

instance BitcodeInput File where
  bitcodeMemoryBuffer (File p) = encodeM (MB.File p)

-- | parse 'Module' from LLVM bitcode. May throw 'ParseFailureException'.
withModuleFromBitcode :: BitcodeInput b => Context -> b -> (Module -> IO a) -> IO a
withModuleFromBitcode (Context c) b f = flip runAnyContT return $ do
  mb <- bitcodeMemoryBuffer b
  msgPtr <- alloca
  m <- anyContToM $ bracket (newModule =<< FFI.parseBitcode c mb msgPtr) (FFI.disposeModule <=< readModule)
  m' <- readModule m
  when (m' == nullPtr) $ throwM . ParseFailureException =<< decodeM msgPtr
  liftIO $ f m

-- | generate LLVM bitcode from a 'Module'
moduleBitcode :: Module -> IO BS.ByteString
moduleBitcode m = do
  m' <- readModule m
  withBufferRawOStream (FFI.writeBitcode m')

-- | write LLVM bitcode from a 'Module' into a file
writeBitcodeToFile :: File -> Module -> IO ()
writeBitcodeToFile (File path) m = flip runAnyContT return $ do
  m' <- readModule m
  withFileRawOStream path False False $ FFI.writeBitcode m'

-- | May throw 'TargetMachineEmitException'.
targetMachineEmit :: FFI.CodeGenFileType -> TargetMachine -> Module -> Ptr FFI.RawPWriteStream -> IO ()
targetMachineEmit fileType (TargetMachine tm) m os = flip runAnyContT return $ do
  msgPtr <- alloca
  m' <- readModule m
  r <- decodeM =<< (liftIO $ FFI.targetMachineEmit tm m' os fileType msgPtr)
  when r $ throwM . TargetMachineEmitException =<< decodeM msgPtr

-- | May throw 'FdStreamException' and 'TargetMachineEmitException'.
emitToFile :: FFI.CodeGenFileType -> TargetMachine -> File -> Module -> IO ()
emitToFile fileType tm (File path) m = flip runAnyContT return $ do
  withFileRawPWriteStream path False False $ targetMachineEmit fileType tm m

-- | May throw 'TargetMachineEmitException'.
emitToByteString :: FFI.CodeGenFileType -> TargetMachine -> Module -> IO BS.ByteString
emitToByteString fileType tm m = flip runAnyContT return $ do
  withBufferRawPWriteStream $ targetMachineEmit fileType tm m

-- | write target-specific assembly directly into a file
writeTargetAssemblyToFile :: TargetMachine -> File -> Module -> IO ()
writeTargetAssemblyToFile = emitToFile FFI.codeGenFileTypeAssembly

-- | produce target-specific assembly as a 'ByteString'
moduleTargetAssembly :: TargetMachine -> Module -> IO ByteString
moduleTargetAssembly tm m = emitToByteString FFI.codeGenFileTypeAssembly tm m

-- | produce target-specific object code as a 'ByteString'
moduleObject :: TargetMachine -> Module -> IO BS.ByteString
moduleObject = emitToByteString FFI.codeGenFileTypeObject

-- | write target-specific object code directly into a file
writeObjectToFile :: TargetMachine -> File -> Module -> IO ()
writeObjectToFile = emitToFile FFI.codeGenFileTypeObject

setTargetTriple :: Ptr FFI.Module -> ShortByteString -> EncodeAST ()
setTargetTriple m t = do
  t <- encodeM t
  liftIO $ FFI.setTargetTriple m t

getTargetTriple :: Ptr FFI.Module -> IO (Maybe ShortByteString)
getTargetTriple m = do
  s <- decodeM =<< liftIO (FFI.getTargetTriple m)
  return $ if SBS.null s then Nothing else Just s

setDataLayout :: Ptr FFI.Module -> A.DataLayout -> EncodeAST ()
setDataLayout m dl = do
  s <- encodeM (dataLayoutToString dl)
  liftIO $ FFI.setDataLayout m s

getDataLayout :: Ptr FFI.Module -> IO (Maybe A.DataLayout)
getDataLayout m = do
  dlString <- decodeM =<< FFI.getDataLayout m
  either fail return . runExcept . parseDataLayout A.BigEndian $ dlString

-- | Execute a function after encoding the module in LLVM’s internal representation.
-- May throw 'EncodeException'.
withModuleFromAST :: Context -> A.Module -> (Module -> IO a) -> IO a
withModuleFromAST context ast =
  bracket (createModuleFromAST context ast) disposeModule

-- | Encode the module AST in LLVM’s internal representation.
-- May throw 'EncodeException'.
createModuleFromAST :: Context -> A.Module -> IO Module
createModuleFromAST context@(Context c) (A.Module moduleId sourceFileName dataLayout triple definitions) = runEncodeAST context $ do
  moduleId <- encodeM moduleId
  m <- liftIO $ newModule =<< FFI.moduleCreateWithNameInContext moduleId c
  ffiMod <- readModule m
  sourceFileName' <- encodeM sourceFileName
  liftIO $ FFI.setSourceFileName ffiMod sourceFileName'
  Context context <- gets encodeStateContext
  traverse_ (setDataLayout ffiMod) dataLayout
  traverse_ (setTargetTriple ffiMod) triple
  let sequencePhases :: EncodeAST [EncodeAST (EncodeAST (EncodeAST (EncodeAST ())))] -> EncodeAST ()
      sequencePhases l = (l >>= (sequence >=> sequence >=> sequence >=> sequence)) >> (return ())
  sequencePhases $ forM definitions $ \d -> case d of
   A.TypeDefinition n t -> do
     (t', n') <- createNamedType n
     defineType n n' t'
     return $ do
       traverse_ (setNamedType t') t
       return . return . return . return $ ()

   A.COMDAT n csk -> do
     n' <- encodeM n
     csk <- encodeM csk
     cd <- liftIO $ FFI.getOrInsertCOMDAT ffiMod n'
     liftIO $ FFI.setCOMDATSelectionKind cd csk
     defineCOMDAT n cd
     return . return . return . return . return $ ()

   A.MetadataNodeDefinition i md -> return . return $ do
     t <- liftIO $ FFI.createTemporaryMDNodeInContext context
     defineMDNode i t
     return $ do
       n <- encodeM md
       liftIO $ FFI.metadataReplaceAllUsesWith (FFI.upCast t) (FFI.upCast n)
       defineMDNode i n
       return $ return ()

   A.NamedMetadataDefinition n ids -> return . return . return . return $ do
     n <- encodeM n
     ids <- encodeM (map A.MDRef ids :: [A.MDRef A.MDNode])
     nm <- liftIO $ FFI.getOrAddNamedMetadata ffiMod n
     liftIO $ FFI.namedMetadataAddOperands nm ids
     return ()

   A.ModuleInlineAssembly s -> do
     s <- encodeM s
     liftIO $ FFI.moduleAppendInlineAsm ffiMod (FFI.ModuleAsm s)
     return . return . return . return . return $ ()

   A.FunctionAttributes gid attrs -> do
     attrs <- encodeM attrs
     defineAttributeGroup gid attrs
     return . return . return . return . return $ ()

   A.GlobalDefinition g -> return . phase $ do
     eg' :: EncodeAST (Ptr FFI.GlobalValue) <- case g of
       g@(A.GlobalVariable { A.G.name = n }) -> do
         typ <- encodeM (A.G.type' g)
         g' <- liftIO $ withName n $ \gName ->
                   FFI.addGlobalInAddressSpace ffiMod typ gName
                          (fromIntegral ((\(A.AddrSpace a) -> a) $ A.G.addrSpace g))
         defineGlobal n g'
         setThreadLocalMode g' (A.G.threadLocalMode g)
         liftIO $ do
           hua <- encodeM (A.G.unnamedAddr g)
           FFI.setUnnamedAddr (FFI.upCast g') hua
           ic <- encodeM (A.G.isConstant g)
           FFI.setGlobalConstant g' ic
         return $ do
           traverse_ ((liftIO . FFI.setInitializer g') <=< encodeM) (A.G.initializer g)
           setSection g' (A.G.section g)
           setCOMDAT g' (A.G.comdat g)
           setAlignment g' (A.G.alignment g)
           setMetadata (FFI.upCast g') (A.G.metadata g)
           return (FFI.upCast g')
       (a@A.G.GlobalAlias { A.G.name = n }) -> do
         typ <- encodeM (A.G.type' a)
         as <- encodeM (A.G.addrSpace a)
         a' <- liftIO $ withName n $ \name -> FFI.justAddAlias ffiMod typ as name
         defineGlobal n a'
         liftIO $ do
           hua <- encodeM (A.G.unnamedAddr a)
           FFI.setUnnamedAddr (FFI.upCast a') hua
         return $ do
           setThreadLocalMode a' (A.G.threadLocalMode a)
           (liftIO . FFI.setAliasee a') =<< encodeM (A.G.aliasee a)
           return (FFI.upCast a')
       (A.Function _ _ _ cc rAttrs resultType fName (args, isVarArgs) attrs _ _ _ gc prefix blocks personality metadata) -> do
         typ <- encodeM $ A.FunctionType resultType [t | A.Parameter t _ _ <- args] isVarArgs
         f <- liftIO . withName fName $ \fName -> FFI.addFunction ffiMod fName typ
         defineGlobal fName f
         cc <- encodeM cc
         liftIO $ FFI.setFunctionCallingConvention f cc
         setFunctionAttributes f (AttributeList attrs rAttrs [pa | A.Parameter _ _ pa <- args])
         setPrefixData f prefix
         setSection f (A.G.section g)
         setCOMDAT f (A.G.comdat g)
         setAlignment f (A.G.alignment g)
         setGC f gc
         setPersonalityFn f personality
         forM_ blocks $ \(A.BasicBlock bName _ _) -> do
           b <- liftIO $ withName bName $ \bName -> FFI.appendBasicBlockInContext context f bName
           defineBasicBlock fName bName b
         phase $ do
           let nParams = length args
           ps <- allocaArray nParams
           liftIO $ FFI.getParams f ps
           params <- peekArray nParams ps
           forM_ (zip args params) $ \(A.Parameter _ n _, p) -> do
             defineLocal n p
             n <- encodeM n
             liftIO $ FFI.setValueName (FFI.upCast p) n
           finishInstrs <- forM blocks $ \(A.BasicBlock bName namedInstrs term) -> do
             b <- encodeM bName
             (do
               builder <- gets encodeStateBuilder
               liftIO $ FFI.positionBuilderAtEnd builder b)
             finishes <- mapM encodeM namedInstrs :: EncodeAST [EncodeAST ()]
             void (encodeM term :: EncodeAST (Ptr FFI.Instruction))
             return (sequence_ finishes)
           sequence_ finishInstrs
           locals <- gets $ Map.toList . encodeStateLocals
           forM_ [ n | (n, ForwardValue _) <- locals ] $ \n -> undefinedReference "local" n
           setMetadata (FFI.upCast f) metadata
           return (FFI.upCast f)
     return $ do
       g' <- eg'
       setLinkage g' (A.G.linkage g)
       setVisibility g' (A.G.visibility g)
       setDLLStorageClass g' (A.G.dllStorageClass g)
       return $ return ()
  return m

-- | Destroys a module created by 'createModuleFromAST'.
disposeModule :: Module -> IO ()
disposeModule m = FFI.disposeModule =<< readModule m

-- | Retrieves the context associated with a module.
moduleContext :: Module -> IO Context
moduleContext m = Context <$> (FFI.getModuleContext =<< readModule m)

-- This returns a nested DecodeAST to allow interleaving of different
-- decoding steps. Take a look at the call site in moduleAST for more
-- details.
decodeGlobalVariables :: Ptr FFI.Module -> DecodeAST (DecodeAST [A.G.Global])
decodeGlobalVariables mod = do
  ffiGlobals <- liftIO $ FFI.getXs (FFI.getFirstGlobal mod) FFI.getNextGlobal
  fmap sequence . forM ffiGlobals $ \g -> do
    A.PointerType t as <- typeOf g
    n <- getGlobalName g
    return $
      A.GlobalVariable
        <$> return n
        <*> getLinkage g
        <*> getVisibility g
        <*> getDLLStorageClass g
        <*> getThreadLocalMode g
        <*> (liftIO $ decodeM =<< FFI.getUnnamedAddr (FFI.upCast g))
        <*> (liftIO $ decodeM =<< FFI.isGlobalConstant g)
        <*> return t
        <*> return as
        <*> (do i <- liftIO $ FFI.getInitializer g
                if i == nullPtr
                  then return Nothing
                  else Just <$> decodeM i)
        <*> getSection g
        <*> getCOMDATName g
        <*> getAlignment g
        <*> getMetadata (FFI.upCast g)

-- This returns a nested DecodeAST to allow interleaving of different
-- decoding steps. Take a look at the call site in moduleAST for more
-- details.
decodeGlobalAliases :: Ptr FFI.Module -> DecodeAST (DecodeAST [A.G.Global])
decodeGlobalAliases mod = do
  ffiAliases <- liftIO $ FFI.getXs (FFI.getFirstAlias mod) FFI.getNextAlias
  fmap sequence . forM ffiAliases $ \a -> do
    n <- getGlobalName a
    A.PointerType t as <- typeOf a
    return $
      A.G.GlobalAlias
        <$> return n
        <*> getLinkage a
        <*> getVisibility a
        <*> getDLLStorageClass a
        <*> getThreadLocalMode a
        <*> (liftIO $ decodeM =<< FFI.getUnnamedAddr (FFI.upCast a))
        <*> return t
        <*> return as
        <*> (decodeM =<< (liftIO $ FFI.getAliasee a))

getMetadata :: Ptr FFI.GlobalObject -> DecodeAST [(ShortByteString, A.MDRef A.MDNode)]
getMetadata o = scopeAnyCont $ do
  n <- liftIO (FFI.getNumMetadata o)
  ks <- allocaArray n
  ps <- allocaArray n
  liftIO (FFI.getAllMetadata o ks ps)
  zip <$> decodeM (n, ks) <*> decodeM (n, ps)

setMetadata :: Ptr FFI.GlobalObject -> [(ShortByteString, A.MDRef A.MDNode)] -> EncodeAST ()
setMetadata o md =
  forM_ md $ \(kindName, node) -> do
    kindID <- encodeM kindName
    node <- encodeM node
    liftIO (FFI.setMetadata o kindID node)

-- This returns a nested DecodeAST to allow interleaving of different
-- decoding steps. Take a look at the call site in moduleAST for more
-- details.
decodeFunctions :: Ptr FFI.Module -> DecodeAST (DecodeAST [A.G.Global])
decodeFunctions mod = do
  ffiFunctions <-
    liftIO $ FFI.getXs (FFI.getFirstFunction mod) FFI.getNextFunction
  fmap sequence . forM ffiFunctions $ \f ->
    localScope $ do
      A.PointerType (A.FunctionType returnType _ isVarArg) _ <- typeOf f
      n <- getGlobalName f
      AttributeList fAttrs rAttrs pAttrs <- getAttributeList f
      parameters <- getParameters f pAttrs
      decodeBlocks <- do
        ffiBasicBlocks <-
          liftIO $ FFI.getXs (FFI.getFirstBasicBlock f) FFI.getNextBasicBlock
        fmap sequence . forM ffiBasicBlocks $ \b -> do
          n <- getLocalName b
          decodeInstructions <- getNamedInstructions b
          decodeTerminator <- getBasicBlockTerminator b
          return $
            A.BasicBlock
              <$> return n
              <*> decodeInstructions
              <*> decodeTerminator
      return $
        A.Function
          <$> getLinkage f
          <*> getVisibility f
          <*> getDLLStorageClass f
          <*> (liftIO $ decodeM =<< FFI.getFunctionCallingConvention f)
          <*> return rAttrs
          <*> return returnType
          <*> return n
          <*> return (parameters, isVarArg)
          <*> return fAttrs
          <*> getSection f
          <*> getCOMDATName f
          <*> getAlignment f
          <*> getGC f
          <*> getPrefixData f
          <*> decodeBlocks
          <*> getPersonalityFn f
          <*> getMetadata (FFI.upCast f)

decodeNamedMetadataDefinitions :: Ptr FFI.Module -> DecodeAST [A.Definition]
decodeNamedMetadataDefinitions mod = do
  ffiNamedMetadataNodes <-
    liftIO $ FFI.getXs (FFI.getFirstNamedMetadata mod) FFI.getNextNamedMetadata
  forM ffiNamedMetadataNodes $ \nm ->
    scopeAnyCont $ do
      n <- liftIO $ FFI.getNamedMetadataNumOperands nm
      os <- allocaArray n
      liftIO $ FFI.getNamedMetadataOperands nm os
      A.NamedMetadataDefinition
        <$> (decodeM $ FFI.getNamedMetadataName nm)
        <*> fmap
              (map (\(A.MDRef mid) -> mid))
              (decodeM (n, os) :: DecodeAST [A.MDRef A.MDNode])

-- | Get an LLVM.AST.'LLVM.AST.Module' from a LLVM.'Module' - i.e.
-- raise C++ objects into an Haskell AST.
moduleAST :: Module -> IO A.Module
moduleAST m = runDecodeAST $ do
  mod <- readModule m
  c <- return Context `ap` liftIO (FFI.getModuleContext mod)
  getMetadataKindNames c
  A.Module
    <$> (liftIO $ decodeM =<< FFI.getModuleIdentifier mod)
    <*> (liftIO $ decodeM =<< FFI.getSourceFileName mod)
    <*> (liftIO $ getDataLayout mod)
    <*> (liftIO $ do
           s <- decodeM =<< FFI.getTargetTriple mod
           return $ if SBS.null s then Nothing else Just s)
    <*> (do
      globalDefinitions <-
        map A.GlobalDefinition . concat <$>
        -- Variables, aliases & functions can reference each other. To
        -- resolve this references properly during decoding a two step
        -- process is used: In the first step, the names of the
        -- different definitions are stored. In the second step we can
        -- then decode the definitions and look up the previously
        -- stored references.
        (join . fmap sequence . sequence)
          [ decodeGlobalVariables mod
          , decodeGlobalAliases mod
          , decodeFunctions mod
          ]
      structDefinitions <- getStructDefinitions
      inlineAsm <- decodeM =<< liftIO (FFI.moduleGetInlineAsm mod)
      namedMetadata <- decodeNamedMetadataDefinitions mod
      metadata <- getMetadataDefinitions
      functionAttributes <- do
        functionAttributes <- gets $ functionAttributeListIDs
        forM functionAttributes $ \(as, gid) -> do
          fAttrs <- A.FunctionAttributes <$> return gid <*> decodeM as
          liftIO (FFI.disposeAttributeSet as)
          pure fAttrs
      comdats <- gets $ map (uncurry A.COMDAT) . Map.elems . comdats
      return $
        structDefinitions ++
        inlineAsm ++
        globalDefinitions ++
        namedMetadata ++
        metadata ++
        functionAttributes ++
        comdats)
