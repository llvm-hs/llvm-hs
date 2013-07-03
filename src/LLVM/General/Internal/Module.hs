{-#
  LANGUAGE
  TupleSections,
  ScopedTypeVariables
  #-}

-- | This Haskell module is for/of functions for handling LLVM modules.
module LLVM.General.Internal.Module where

import Control.Monad.Trans
import Control.Monad.State
import Control.Monad.AnyCont
import Control.Applicative
import Control.Exception

import Foreign.Ptr
import Foreign.Marshal.Alloc (free)
import Data.ByteString (ByteString)

import qualified LLVM.General.Internal.FFI.Assembly as FFI
import qualified LLVM.General.Internal.FFI.Builder as FFI
import qualified LLVM.General.Internal.FFI.Function as FFI
import qualified LLVM.General.Internal.FFI.GlobalAlias as FFI
import qualified LLVM.General.Internal.FFI.GlobalValue as FFI
import qualified LLVM.General.Internal.FFI.GlobalVariable as FFI
import qualified LLVM.General.Internal.FFI.Iterate as FFI
import qualified LLVM.General.Internal.FFI.LLVMCTypes as FFI
import qualified LLVM.General.Internal.FFI.MemoryBuffer as FFI
import qualified LLVM.General.Internal.FFI.Metadata as FFI
import qualified LLVM.General.Internal.FFI.Module as FFI
import qualified LLVM.General.Internal.FFI.PtrHierarchy as FFI
import qualified LLVM.General.Internal.FFI.Target as FFI
import qualified LLVM.General.Internal.FFI.Value as FFI

import LLVM.General.Internal.BasicBlock
import LLVM.General.Internal.Coding
import LLVM.General.Internal.Context
import LLVM.General.Internal.DataLayout
import LLVM.General.Internal.DecodeAST
import LLVM.General.Internal.Diagnostic
import LLVM.General.Internal.EncodeAST
import LLVM.General.Internal.Function
import LLVM.General.Internal.Global
import LLVM.General.Internal.Instruction ()
import LLVM.General.Internal.MemoryBuffer ()
import LLVM.General.Internal.Metadata
import LLVM.General.Internal.Operand
import LLVM.General.Internal.String
import LLVM.General.Internal.Target
import LLVM.General.Internal.Type
import LLVM.General.Internal.Value

import LLVM.General.Diagnostic

import qualified LLVM.General.AST as A
import qualified LLVM.General.AST.DataLayout as A
import qualified LLVM.General.AST.AddrSpace as A
import qualified LLVM.General.AST.Global as A.G

-- | <http://llvm.org/doxygen/classllvm_1_1Module.html>
newtype Module = Module (Ptr FFI.Module)

-- | parse 'Module' from LLVM assembly
withModuleFromString :: Context -> String -> (Module -> IO a) -> IO (Either Diagnostic a)
withModuleFromString (Context c) s f = flip runAnyContT return $ do
  s <- encodeM s
  liftIO $ withSMDiagnostic $ \smDiag -> do
    m <- FFI.getModuleFromAssemblyInContext c s smDiag
    if m == nullPtr then
      Left <$> getDiagnostic smDiag
     else
      Right <$> finally (f (Module m)) (FFI.disposeModule m)

-- | generate LLVM assembly from a 'Module'
moduleString :: Module -> IO String
moduleString (Module m) = bracket (FFI.getModuleAssembly m) free $ decodeM

-- | generate LLVM bitcode from a 'Module'
writeBitcodeToFile :: FilePath -> Module -> IO ()
writeBitcodeToFile path (Module m) = flip runAnyContT return $ do
  msgPtr <- alloca
  path <- encodeM path
  result <- decodeM =<< (liftIO $ FFI.writeBitcodeToFile m path msgPtr)
  when result $ do
    msg <- anyContT $ bracket (peek msgPtr) free
    fail =<< decodeM msg

emitToFile :: FFI.CodeGenFileType -> TargetMachine -> FilePath -> Module -> IO ()
emitToFile fileType (TargetMachine tm) path (Module m) = flip runAnyContT return $ do
  msgPtr <- alloca
  path <- encodeM path
  result <- decodeM =<< (liftIO $ FFI.targetMachineEmitToFile tm m path fileType msgPtr)
  when result $ fail =<< decodeM =<< anyContT (bracket (peek msgPtr) free)

writeAssemblyToFile :: TargetMachine -> FilePath -> Module -> IO ()
writeAssemblyToFile = emitToFile FFI.codeGenFileTypeAssembly

writeObjectToFile :: TargetMachine -> FilePath -> Module -> IO ()
writeObjectToFile = emitToFile FFI.codeGenFileTypeObject

emitToByteString :: FFI.CodeGenFileType -> TargetMachine -> Module -> IO ByteString
emitToByteString fileType (TargetMachine tm) (Module m) = flip runAnyContT return $ do
  msgPtr <- alloca
  memoryBufferPtr <- alloca
  result <- decodeM =<< (liftIO $ FFI.targetMachineEmitToMemoryBuffer tm m fileType msgPtr memoryBufferPtr)
  if result 
    then
        fail =<< decodeM =<< anyContT (bracket (peek msgPtr) free)
    else
        decodeM =<< anyContT (bracket (peek memoryBufferPtr) FFI.disposeMemoryBuffer)

moduleAssembly :: TargetMachine -> Module -> IO String
moduleAssembly tm m = decodeM . UTF8ByteString =<< emitToByteString FFI.codeGenFileTypeAssembly tm m

moduleObject :: TargetMachine -> Module -> IO ByteString
moduleObject = emitToByteString FFI.codeGenFileTypeObject

setTargetTriple :: Ptr FFI.Module -> String -> IO ()
setTargetTriple m t = flip runAnyContT return $ do
  t <- encodeM t
  liftIO $ FFI.setTargetTriple m t

getTargetTriple :: Ptr FFI.Module -> IO (Maybe String)
getTargetTriple m = do
  s <- decodeM =<< liftIO (FFI.getTargetTriple m)
  return $ if s == "" then Nothing else Just s

setDataLayout :: Ptr FFI.Module -> A.DataLayout -> IO ()
setDataLayout m dl = flip runAnyContT return $ do
  s <- encodeM (dataLayoutToString dl)
  liftIO $ FFI.setDataLayout m s

getDataLayout :: Ptr FFI.Module -> IO (Maybe A.DataLayout)
getDataLayout m = parseDataLayout <$> (decodeM =<< FFI.getDataLayout m)

type P a = a -> a

-- | Build an LLVM.General.'Module' from a LLVM.General.AST.'LLVM.General.AST.Module' - i.e.
-- lower an AST from Haskell into C++ objects.
withModuleFromAST :: Context -> A.Module -> (Module -> IO a) -> IO (Either String a)
withModuleFromAST context@(Context c) (A.Module moduleId dataLayout triple definitions) f = do
  let makeModule = flip runAnyContT return $ do
                     moduleId <- encodeM moduleId
                     liftIO $ FFI.moduleCreateWithNameInContext moduleId c
  bracket makeModule FFI.disposeModule $ \m -> do
    maybe (return ()) (setDataLayout m) dataLayout
    maybe (return ()) (setTargetTriple m) triple
    let sequencePhases :: EncodeAST [EncodeAST (EncodeAST (EncodeAST (EncodeAST ())))] -> EncodeAST ()
        sequencePhases l = (l >>= (sequence >=> sequence >=> sequence >=> sequence)) >> (return ())

    r <- runEncodeAST context $ sequencePhases $ forM definitions $ \d -> case d of
      A.TypeDefinition n t -> do
        t' <- createNamedType n
        defineType n t'
        return $ do
          maybe (return ()) (setNamedType t') t
          return . return . return $ return ()

      A.MetadataNodeDefinition i os -> return . return $ do
        t <- liftIO $ FFI.createTemporaryMDNodeInContext c
        defineMDNode i t
        return $ do
          n <- encodeM (A.MetadataNode os)
          liftIO $ FFI.replaceAllUsesWith (FFI.upCast t) (FFI.upCast n)
          defineMDNode i n
          liftIO $ FFI.destroyTemporaryMDNode t
          return $ return ()

      A.NamedMetadataDefinition n ids -> return . return . return . return $ do
        n <- encodeM n
        ids <- encodeM (map A.MetadataNodeReference ids)
        nm <- liftIO $ FFI.getOrAddNamedMetadata m n
        liftIO $ FFI.namedMetadataAddOperands nm ids
        return ()

      A.ModuleInlineAssembly s -> do
        s <- encodeM s
        liftIO $ FFI.moduleAppendInlineAsm m (FFI.ModuleAsm s)
        return . return . return . return $ return ()

      A.GlobalDefinition g -> return . phase $ do
        eg' :: EncodeAST (Ptr FFI.GlobalValue) <- case g of
          g@(A.GlobalVariable { A.G.name = n }) -> do
            typ <- encodeM (A.G.type' g)
            g' <- liftIO $ withName n $ \gName -> 
                      FFI.addGlobalInAddressSpace m typ gName 
                             (fromIntegral ((\(A.AddrSpace a) -> a) $ A.G.addrSpace g))
            defineGlobal n g'
            liftIO $ do
              tl <- encodeM (A.G.isThreadLocal g)
              FFI.setThreadLocal g' tl
              hua <- encodeM (A.G.hasUnnamedAddr g)
              FFI.setUnnamedAddr (FFI.upCast g') hua
              ic <- encodeM (A.G.isConstant g)
              FFI.setGlobalConstant g' ic
            return $ do
              maybe (return ()) ((liftIO . FFI.setInitializer g') <=< encodeM) (A.G.initializer g)
              setSection g' (A.G.section g)
              setAlignment g' (A.G.alignment g)
              return (FFI.upCast g')
          (a@A.G.GlobalAlias { A.G.name = n }) -> do
            typ <- encodeM (A.G.type' a)
            a' <- liftIO $ withName n $ \name -> FFI.justAddAlias m typ name
            defineGlobal n a'
            return $ do
              (liftIO . FFI.setAliasee a') =<< encodeM (A.G.aliasee a)
              return (FFI.upCast a')
          (A.Function _ _ cc rAttrs resultType fName (args,isVarArgs) attrs _ _ blocks) -> do
            typ <- encodeM $ A.FunctionType resultType (map (\(A.Parameter t _ _) -> t) args) isVarArgs
            f <- liftIO . withName fName $ \fName -> FFI.addFunction m fName typ
            defineGlobal fName f
            cc <- encodeM cc
            liftIO $ FFI.setFunctionCallConv f cc
            rAttrs <- encodeM rAttrs
            liftIO $ FFI.addFunctionRetAttr f rAttrs
            liftIO $ setFunctionAttrs f attrs
            setSection f (A.G.section g)
            setAlignment f (A.G.alignment g)
            forM blocks $ \(A.BasicBlock bName _ _) -> do
              b <- liftIO $ withName bName $ \bName -> FFI.appendBasicBlockInContext c f bName
              defineBasicBlock fName bName b
            phase $ do
              let nParams = length args
              ps <- allocaArray nParams
              liftIO $ FFI.getParams f ps
              params <- peekArray nParams ps
              forM (zip args params) $ \(A.Parameter _ n attrs, p) -> do
                defineLocal n p
                n <- encodeM n
                liftIO $ FFI.setValueName (FFI.upCast p) n
                unless (null attrs) $
                       do attrs <- encodeM attrs
                          liftIO $ FFI.addAttribute p attrs
                          return ()
                return ()
              finishInstrs <- forM blocks $ \(A.BasicBlock bName namedInstrs term) -> do
                b <- encodeM bName
                (do
                  builder <- gets encodeStateBuilder
                  liftIO $ FFI.positionBuilderAtEnd builder b)
                finishes <- mapM encodeM namedInstrs :: EncodeAST [EncodeAST ()]
                (encodeM term :: EncodeAST (Ptr FFI.Instruction))
                return (sequence_ finishes)
              sequence_ finishInstrs
              return (FFI.upCast f)
        return $ do
          g' <- eg'
          setLinkage g' (A.G.linkage g)
          setVisibility g' (A.G.visibility g)
          return $ return ()
          
    either (return . Left) (const $ Right <$> f (Module m)) r

-- | Get an LLVM.General.AST.'LLVM.General.AST.Module' from a LLVM.General.'Module' - i.e.
-- raise C++ objects into an Haskell AST.
moduleAST :: Module -> IO A.Module
moduleAST (Module mod) = runDecodeAST $ do
  c <- return Context `ap` liftIO (FFI.getModuleContext mod)
  getMetadataKindNames c
  return A.Module 
   `ap` (liftIO $ bracket (FFI.getModuleIdentifier mod) free decodeM)
   `ap` (liftIO $ getDataLayout mod)
   `ap` (liftIO $ do
           s <- decodeM <=< FFI.getTargetTriple $ mod
           return $ if s == "" then Nothing else Just s)
   `ap` (
     do
       gs <- map A.GlobalDefinition . concat <$> (join . liftM sequence . sequence) [
          do
            ffiGlobals <- liftIO $ FFI.getXs (FFI.getFirstGlobal mod) FFI.getNextGlobal
            liftM sequence . forM ffiGlobals $ \g -> do
              A.PointerType t as <- typeOf g
              n <- getGlobalName g
              return $ return A.GlobalVariable
               `ap` return n
               `ap` getLinkage g
               `ap` getVisibility g
               `ap` (liftIO $ decodeM =<< FFI.isThreadLocal g)
               `ap` return as
               `ap` (liftIO $ decodeM =<< FFI.hasUnnamedAddr (FFI.upCast g))
               `ap` (liftIO $ decodeM =<< FFI.isGlobalConstant g)
               `ap` return t
               `ap` (do
                      i <- liftIO $ FFI.getInitializer g
                      if i == nullPtr then return Nothing else Just <$> decodeM i)
               `ap` getSection g
               `ap` getAlignment g,

          do
            ffiAliases <- liftIO $ FFI.getXs (FFI.getFirstAlias mod) FFI.getNextAlias
            liftM sequence . forM ffiAliases $ \a -> do
              n <- getGlobalName a
              return $ return A.G.GlobalAlias
               `ap` return n
               `ap` getLinkage a
               `ap` getVisibility a
               `ap` typeOf a
               `ap` (decodeM =<< (liftIO $ FFI.getAliasee a)),

          do
            ffiFunctions <- liftIO $ FFI.getXs (FFI.getFirstFunction mod) FFI.getNextFunction
            liftM sequence . forM ffiFunctions $ \f -> localScope $ do
              A.PointerType (A.FunctionType returnType _ isVarArg) _ <- typeOf f
              n <- getGlobalName f
              parameters <- getParameters f
              decodeBlocks <- do
                ffiBasicBlocks <- liftIO $ FFI.getXs (FFI.getFirstBasicBlock f) FFI.getNextBasicBlock
                liftM sequence . forM ffiBasicBlocks $ \b -> do
                  n <- getLocalName b
                  decodeInstructions <- getNamedInstructions b
                  decodeTerminator <- getBasicBlockTerminator b
                  return $ return A.BasicBlock `ap` return n `ap` decodeInstructions `ap` decodeTerminator
              return $ return A.Function
                 `ap` getLinkage f
                 `ap` getVisibility f
                 `ap` (liftIO $ decodeM =<< FFI.getFunctionCallConv f)
                 `ap` (liftIO $ decodeM =<< FFI.getFunctionRetAttr f)
                 `ap` return returnType
                 `ap` return n
                 `ap` return (parameters, isVarArg)
                 `ap` (liftIO $ getFunctionAttrs f)
                 `ap` getSection f
                 `ap` getAlignment f
                 `ap` decodeBlocks
        ]

       tds <- getStructDefinitions

       ias <- decodeM =<< liftIO (FFI.moduleGetInlineAsm mod)

       nmds <- do
         ffiNamedMetadataNodes <- liftIO $ FFI.getXs (FFI.getFirstNamedMetadata mod) FFI.getNextNamedMetadata
         forM ffiNamedMetadataNodes $ \nm -> scopeAnyCont $ do
              n <- liftIO $ FFI.getNamedMetadataNumOperands nm
              os <- allocaArray n
              liftIO $ FFI.getNamedMetadataOperands nm os
              return A.NamedMetadataDefinition
                 `ap` (decodeM $ FFI.getNamedMetadataName nm)
                 `ap` liftM (map (\(A.MetadataNodeReference mid) -> mid)) (decodeM (n, os))
         
       mds <- getMetadataDefinitions

       return $ tds ++ ias ++ gs ++ nmds ++ mds
   )
