{-# LANGUAGE
  TemplateHaskell,
  QuasiQuotes,
  MultiParamTypeClasses,
  ScopedTypeVariables
  #-}
module LLVM.General.Internal.Constant where

import LLVM.General.Prelude

import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Quote as TH
import qualified LLVM.General.Internal.InstructionDefs as ID

import Data.Bits
import Control.Monad.State (get, gets, modify, evalState)
import Control.Monad.AnyCont
import Control.Monad.IO.Class

import qualified Data.Map as Map
import Foreign.Ptr
import Foreign.Storable (Storable, sizeOf)

import qualified LLVM.General.Internal.FFI.Constant as FFI
import qualified LLVM.General.Internal.FFI.GlobalValue as FFI
import qualified LLVM.General.Internal.FFI.Instruction as FFI
import qualified LLVM.General.Internal.FFI.LLVMCTypes as FFI
import LLVM.General.Internal.FFI.LLVMCTypes (valueSubclassIdP)
import qualified LLVM.General.Internal.FFI.PtrHierarchy as FFI
import qualified LLVM.General.Internal.FFI.Type as FFI
import qualified LLVM.General.Internal.FFI.User as FFI
import qualified LLVM.General.Internal.FFI.Value as FFI
import qualified LLVM.General.Internal.FFI.BinaryOperator as FFI

import qualified LLVM.General.AST.Constant as A (Constant)
import qualified LLVM.General.AST.Constant as A.C hiding (Constant)
import qualified LLVM.General.AST.Type as A
import qualified LLVM.General.AST.IntegerPredicate as A (IntegerPredicate)
import qualified LLVM.General.AST.FloatingPointPredicate as A (FloatingPointPredicate)
import qualified LLVM.General.AST.Float as A.F

import LLVM.General.Internal.Coding
import LLVM.General.Internal.DecodeAST
import LLVM.General.Internal.EncodeAST
import LLVM.General.Internal.Context
import LLVM.General.Internal.Type ()
import LLVM.General.Internal.IntegerPredicate ()
import LLVM.General.Internal.FloatingPointPredicate ()

allocaWords :: forall a m . (Storable a, MonadAnyCont IO m, Monad m, MonadIO m) => Word32 -> m (Ptr a)
allocaWords nBits = do
  allocaArray (((nBits-1) `div` (8*(fromIntegral (sizeOf (undefined :: a))))) + 1)

instance EncodeM EncodeAST A.Constant (Ptr FFI.Constant) where
  encodeM c = scopeAnyCont $ case c of
    A.C.Int { A.C.integerBits = bits, A.C.integerValue = v } -> do
      t <- encodeM (A.IntegerType bits)
      words <- encodeM [
        fromIntegral ((v `shiftR` (w*64)) .&. 0xffffffffffffffff) :: Word64
        | w <- [0 .. ((fromIntegral bits-1) `div` 64)] 
       ]
      liftIO $ FFI.constantIntOfArbitraryPrecision t words
    A.C.Float { A.C.floatValue = v } -> do
      Context context <- gets encodeStateContext
      let poke1 f = do
            let nBits = fromIntegral $ 8*(sizeOf f)
            words <- allocaWords nBits
            poke (castPtr words) f
            return (nBits, words)
          poke2 fh fl = do
             let nBits = fromIntegral $ 8*(sizeOf fh) + 8*(sizeOf fl)
             words <- allocaWords nBits
             pokeByteOff (castPtr words) 0 fl
             pokeByteOff (castPtr words) (sizeOf fl) fh
             return (nBits, words)
      (nBits, words) <- case v of
        A.F.Half f -> poke1 f
        A.F.Single f -> poke1 f
        A.F.Double f -> poke1 f
        A.F.X86_FP80 high low -> poke2 high low
        A.F.Quadruple high low -> poke2 high low
        A.F.PPC_FP128 high low -> poke2 high low
      let fpSem = case v of
                    A.F.Half _ -> FFI.floatSemanticsIEEEhalf
                    A.F.Single _ -> FFI.floatSemanticsIEEEsingle
                    A.F.Double _ -> FFI.floatSemanticsIEEEdouble
                    A.F.Quadruple _ _ -> FFI.floatSemanticsIEEEquad
                    A.F.X86_FP80 _ _ -> FFI.floatSemanticsx87DoubleExtended
                    A.F.PPC_FP128 _ _ -> FFI.floatSemanticsPPCDoubleDouble
      nBits <- encodeM nBits
      liftIO $ FFI.constantFloatOfArbitraryPrecision context nBits words fpSem
    A.C.GlobalReference _ n -> FFI.upCast <$> referGlobal n
    A.C.BlockAddress f b -> do
      f' <- referGlobal f
      b' <- getBlockForAddress f b
      liftIO $ FFI.blockAddress (FFI.upCast f') b'
    A.C.Struct nm p ms -> do
      p <- encodeM p
      ms <- encodeM ms
      case nm of
        Nothing -> do
          Context context <- gets encodeStateContext
          liftIO $ FFI.constStructInContext context ms p
        Just nm -> do
          t <- lookupNamedType nm
          liftIO $ FFI.constNamedStruct t ms
    o -> $(do
      let constExprInfo =  ID.outerJoin ID.astConstantRecs (ID.innerJoin ID.astInstructionRecs ID.instructionDefs)
      TH.caseE [| o |] $ do
        (name, (Just (TH.RecC n fs), instrInfo)) <- Map.toList constExprInfo
        let fns = [ TH.mkName . TH.nameBase $ fn | (fn, _, _) <- fs ]
            coreCall n = TH.dyn $ "FFI.constant" ++ n
            buildBody c = [ TH.bindS (TH.varP fn) [| encodeM $(TH.varE fn) |] | fn <- fns ]
                          ++ [ TH.noBindS [| liftIO $(foldl TH.appE c (map TH.varE fns)) |] ]
            hasFlags = any (== ''Bool) [ h | (_, _, TH.ConT h) <- fs ]
        core <- case instrInfo of
          Just (_, iDef) -> do
            let opcode = TH.dataToExpQ (const Nothing) (ID.cppOpcode iDef)
            case ID.instructionKind iDef of
              ID.Binary | hasFlags -> return $ coreCall name
                        | True -> return [| $(coreCall "BinaryOperator") $(opcode) |]
              ID.Cast -> return [| $(coreCall "Cast") $(opcode) |]
              _ -> return $ coreCall name
          Nothing -> if (name `elem` ["Vector", "Null", "Array", "Undef"])
                      then return $ coreCall name
                      else []
        return $ TH.match
          (TH.recP n [(fn,) <$> (TH.varP . TH.mkName . TH.nameBase $ fn) | (fn, _, _) <- fs])
          (TH.normalB (TH.doE (buildBody core)))
          []
      )

instance DecodeM DecodeAST A.Constant (Ptr FFI.Constant) where
  decodeM c = scopeAnyCont $ do
    let v = FFI.upCast c :: Ptr FFI.Value
        u = FFI.upCast c :: Ptr FFI.User
    ft <- liftIO (FFI.typeOf v)
    t <- decodeM ft
    valueSubclassId <- liftIO $ FFI.getValueSubclassId v
    nOps <- liftIO $ FFI.getNumOperands u
    let globalRef = return A.C.GlobalReference 
                    `ap` (return t)
                    `ap` (getGlobalName =<< liftIO (FFI.isAGlobalValue v))
        op = decodeM <=< liftIO . FFI.getConstantOperand c
        getConstantOperands = mapM op [0..nOps-1] 
        getConstantData = do
          let nElements = case t of
                            A.VectorType n _ -> n
                            A.ArrayType n _ | n <= (fromIntegral (maxBound :: Word32)) -> fromIntegral n
          forM [0..nElements-1] $ do
             decodeM <=< liftIO . FFI.getConstantDataSequentialElementAsConstant c . fromIntegral

    case valueSubclassId of
      [valueSubclassIdP|Function|] -> globalRef
      [valueSubclassIdP|GlobalAlias|] -> globalRef
      [valueSubclassIdP|GlobalVariable|] -> globalRef
      [valueSubclassIdP|ConstantInt|] -> do
        np <- alloca
        wsp <- liftIO $ FFI.getConstantIntWords c np
        n <- peek np
        words <- decodeM (n, wsp)
        return $ A.C.Int (A.typeBits t) (foldr (\b a -> (a `shiftL` 64) .|. fromIntegral b) 0 (words :: [Word64]))
      [valueSubclassIdP|ConstantFP|] -> do
        let A.FloatingPointType nBits fmt = t
        ws <- allocaWords nBits
        liftIO $ FFI.getConstantFloatWords c ws
        A.C.Float <$> (
          case (nBits, fmt) of
            (16, A.IEEE) -> A.F.Half <$> peek (castPtr ws)
            (32, A.IEEE) -> A.F.Single <$> peek (castPtr ws)
            (64, A.IEEE) -> A.F.Double <$> peek (castPtr ws)
            (128, A.IEEE) -> A.F.Quadruple <$> peekByteOff (castPtr ws) 8 <*> peekByteOff (castPtr ws) 0
            (80, A.DoubleExtended) -> A.F.X86_FP80 <$> peekByteOff (castPtr ws) 8 <*> peekByteOff (castPtr ws) 0
            (128, A.PairOfFloats) -> A.F.PPC_FP128 <$> peekByteOff (castPtr ws) 8 <*> peekByteOff (castPtr ws) 0
            _ -> error $ "don't know how to decode floating point constant of type: " ++ show t
          )
      [valueSubclassIdP|ConstantPointerNull|] -> return $ A.C.Null t
      [valueSubclassIdP|ConstantAggregateZero|] -> return $ A.C.Null t
      [valueSubclassIdP|UndefValue|] -> return $ A.C.Undef t
      [valueSubclassIdP|BlockAddress|] -> 
            return A.C.BlockAddress 
               `ap` (getGlobalName =<< do liftIO $ FFI.isAGlobalValue =<< FFI.getBlockAddressFunction c)
               `ap` (getLocalName =<< do liftIO $ FFI.getBlockAddressBlock c)
      [valueSubclassIdP|ConstantStruct|] -> do
            return A.C.Struct
               `ap` (return $ case t of A.NamedTypeReference n -> Just n; _ -> Nothing)
               `ap` (decodeM =<< liftIO (FFI.isPackedStruct ft))
               `ap` getConstantOperands
      [valueSubclassIdP|ConstantDataArray|] -> 
            return A.C.Array `ap` (return $ A.elementType t) `ap` getConstantData
      [valueSubclassIdP|ConstantArray|] -> 
            return A.C.Array `ap` (return $ A.elementType t) `ap` getConstantOperands
      [valueSubclassIdP|ConstantDataVector|] -> 
            return A.C.Vector `ap` getConstantData
      [valueSubclassIdP|ConstantExpr|] -> do
            cppOpcode <- liftIO $ FFI.getConstantCPPOpcode c
            $(
              TH.caseE [| cppOpcode |] $ do
                (name, ((TH.RecC n fs, _), iDef)) <- Map.toList $
                      ID.innerJoin (ID.innerJoin ID.astConstantRecs ID.astInstructionRecs) ID.instructionDefs
                let apWrapper o (fn, _, ct) = do
                      a <- case ct of
                             TH.ConT h
                               | h == ''A.Constant -> do
                                               operandNumber <- get
                                               modify (+1)
                                               return [| op $(TH.litE . TH.integerL $ operandNumber) |]
                               | h == ''A.Type -> return [| pure t |]
                               | h == ''A.IntegerPredicate -> 
                                 return [| liftIO $ decodeM =<< FFI.getConstantICmpPredicate c |]
                               | h == ''A.FloatingPointPredicate -> 
                                 return [| liftIO $ decodeM =<< FFI.getConstantFCmpPredicate c |]
                               | h == ''Bool -> case TH.nameBase fn of
                                                  "inBounds" -> return [| liftIO $ decodeM =<< FFI.getInBounds v |]
                                                  "exact" -> return [| liftIO $ decodeM =<< FFI.isExact v |]
                                                  "nsw" -> return [| liftIO $ decodeM =<< FFI.hasNoSignedWrap v |]
                                                  "nuw" -> return [| liftIO $ decodeM =<< FFI.hasNoUnsignedWrap v |]
                                                  x -> error $ "constant bool field " ++ show x ++ " not handled yet"
                             TH.AppT TH.ListT (TH.ConT h) 
                               | h == ''Word32 -> 
                                  return [|
                                        do
                                          np <- alloca
                                          isp <- liftIO $ FFI.getConstantIndices c np
                                          n <- peek np
                                          decodeM (n, isp)
                                        |]
                               | h == ''A.Constant -> 
                                  case TH.nameBase fn of
                                    "indices" -> do
                                      operandNumber <- get
                                      return [| mapM op [$(TH.litE . TH.integerL $ operandNumber)..nOps-1] |]
                             _ -> error $ "unhandled constant expr field type: " ++ show fn ++ " - " ++ show ct
                      return [| $(o) `ap` $(a) |]
                return $ TH.match 
                          (TH.dataToPatQ (const Nothing) (ID.cppOpcode iDef))
                          (TH.normalB (evalState (foldM apWrapper [| return $(TH.conE n) |] fs) 0))
                          []
             )
      _ -> error $ "unhandled constant valueSubclassId: " ++ show valueSubclassId


  
  
