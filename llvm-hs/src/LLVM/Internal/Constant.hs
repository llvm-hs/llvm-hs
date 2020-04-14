{-# LANGUAGE
  TemplateHaskell,
  QuasiQuotes,
  MultiParamTypeClasses,
  ScopedTypeVariables
  #-}
module LLVM.Internal.Constant where

import LLVM.Prelude

import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Quote as TH
import qualified LLVM.Internal.InstructionDefs as ID

import Data.Bits
import Control.Monad.AnyCont
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.State (get, gets, modify, evalState)

import qualified Data.Map as Map
import Foreign.Ptr
import Foreign.Storable (Storable, sizeOf)

import qualified LLVM.Internal.FFI.Constant as FFI
import qualified LLVM.Internal.FFI.GlobalValue as FFI
import qualified LLVM.Internal.FFI.Instruction as FFI
import qualified LLVM.Internal.FFI.LLVMCTypes as FFI
import LLVM.Internal.FFI.LLVMCTypes (valueSubclassIdP)
import qualified LLVM.Internal.FFI.PtrHierarchy as FFI
import qualified LLVM.Internal.FFI.Type as FFI
import qualified LLVM.Internal.FFI.User as FFI
import qualified LLVM.Internal.FFI.Value as FFI
import qualified LLVM.Internal.FFI.BinaryOperator as FFI

import qualified LLVM.AST.Constant as A (Constant)
import qualified LLVM.AST.Constant as A.C hiding (Constant)
import qualified LLVM.AST.Type as A
import qualified LLVM.AST.IntegerPredicate as A (IntegerPredicate)
import qualified LLVM.AST.FloatingPointPredicate as A (FloatingPointPredicate)
import qualified LLVM.AST.Float as A.F

import LLVM.Exception
import LLVM.Internal.Coding
import LLVM.Internal.Context
import LLVM.Internal.DecodeAST
import LLVM.Internal.EncodeAST
import LLVM.Internal.FloatingPointPredicate ()
import LLVM.Internal.IntegerPredicate ()
import LLVM.Internal.Type (renameType)
import LLVM.Internal.Value

allocaWords :: forall a m . (Storable a, MonadAnyCont IO m, Monad m, MonadIO m) => Word32 -> m (Ptr a)
allocaWords nBits = do
  allocaArray (((nBits-1) `div` (8*(fromIntegral (sizeOf (undefined :: a))))) + 1)

inconsistentCases :: Show a => String -> a -> b
inconsistentCases name attr =
  error $ "llvm-hs internal error: cases inconstistent in " ++ name ++ " encoding for " ++ show attr

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
    A.C.GlobalReference ty n -> do
      ref <- FFI.upCast <$> referGlobal n
      ty' <- (liftIO . runDecodeAST . typeOf) ref
      renamedTy <- renameType ty
      if renamedTy /= ty'
        then throwM
               (EncodeException
                  ("The serialized GlobalReference " ++ show n  ++ " has type " ++ show ty ++ " but should have type " ++ show ty'))
        else return ref
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
    A.C.TokenNone -> do
      Context context <- gets encodeStateContext
      liftIO $ FFI.getConstTokenNone context
    o -> $(do
      let -- This is a mapping from constructor names to the constructor of the constant
          -- and the constructor and the definition of the instruction.
          constExprInfo :: Map.Map String (Maybe TH.Con, Maybe (TH.Con, ID.InstructionDef))
          constExprInfo =  ID.outerJoin ID.astConstantRecs (ID.innerJoin ID.astInstructionRecs ID.instructionDefs)
      TH.caseE [| o |] $
        map (\p -> TH.match p (TH.normalB [|inconsistentCases "Constant" o|]) [])
            [[p|A.C.Int{}|],
             [p|A.C.Float{}|],
             [p|A.C.Struct{}|],
             [p|A.C.BlockAddress{}|],
             [p|A.C.GlobalReference{}|],
             [p|A.C.TokenNone{}|]] ++
        (do (name, (Just (TH.RecC n fields), instrInfo)) <- Map.toList constExprInfo
            let fieldNames = [ TH.mkName . TH.nameBase $ fn | (fn, _, _) <- fields ]
                coreCall n = TH.dyn $ "FFI.constant" ++ n
                -- Addition validations that are run during encoding. A common usage of
                -- this is to check if certain types are allowed. The record fields are in scope
                -- when the validations are run.
                validations = case name of
                  "Null" ->
                    [ TH.noBindS
                        [| case $(TH.dyn "constantType") of
                             A.PointerType {} -> pure ()
                             _ ->
                               throwM
                                 (EncodeException
                                    ("Null pointer constant must have pointer type but has type " <>
                                     show $(TH.dyn "constantType") <> "."))
                        |]
                    ]
                  "AggregateZero" ->
                    [ TH.noBindS $
                        [| case $(TH.dyn "constantType") of
                             A.ArrayType {} -> pure ()
                             A.StructureType {} -> pure ()
                             A.VectorType {} -> pure ()
                             A.NamedTypeReference {} -> pure ()
                             _ ->
                               throwM
                                 (EncodeException
                                    ("Aggregate zero constant must have struct, array or vector type but has type " <>
                                     show $(TH.dyn "constantType") <> "."))
                        |]
                    ]
                  _ -> []
                buildBody c =
                  validations ++
                  [ TH.bindS (TH.varP fn) [| encodeM $(TH.varE fn) |] | fn <- fieldNames ] ++
                  [ TH.noBindS [| liftIO $(foldl TH.appE c (map TH.varE fieldNames)) |] ]
                hasFlags = ''Bool `elem` [ h | (_, _, TH.ConT h) <- fields ]
            core <- case instrInfo of
              Just (_, iDef) -> do
                let opcode = TH.dataToExpQ (const Nothing) (ID.cppOpcode iDef)
                case ID.instructionKind iDef of
                  ID.Binary
                    | hasFlags -> return $ coreCall name
                    | otherwise -> return [| $(coreCall "BinaryOperator") $(opcode) |]
                  ID.Cast -> return [| $(coreCall "Cast") $(opcode) |]
                  _ -> return $ coreCall name
              Nothing ->
                case name of
                  "Array" -> pure (TH.varE 'FFI.constantArray)
                  "AggregateZero" -> pure (TH.varE 'FFI.constantNull)
                  "Null" -> pure (TH.varE 'FFI.constantNull)
                  "Undef" -> pure (TH.varE 'FFI.constantUndef)
                  "Vector" -> pure (TH.varE 'FFI.constantVector)
                  _ -> [] -- We have already handled these values
            return $ TH.match
              (TH.recP n [(fn,) <$> (TH.varP . TH.mkName . TH.nameBase $ fn) | (fn, _, _) <- fields])
              (TH.normalB (TH.doE (buildBody core)))
              [])
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
          let nElements =
                case t of
                  A.VectorType n _ -> n
                  A.ArrayType n _ | n <= (fromIntegral (maxBound :: Word32)) -> fromIntegral n
                  _ -> error "getConstantData can only be applied to vectors and arrays"
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
        let A.FloatingPointType fpt = t
        let nBits = case fpt of
                A.HalfFP      -> 16
                A.FloatFP     -> 32
                A.DoubleFP    -> 64
                A.FP128FP     -> 128
                A.X86_FP80FP  -> 80
                A.PPC_FP128FP -> 128
        ws <- allocaWords nBits
        liftIO $ FFI.getConstantFloatWords c ws
        A.C.Float <$> (
          case fpt of
            A.HalfFP      -> A.F.Half <$> peek (castPtr ws)
            A.FloatFP     -> A.F.Single <$> peek (castPtr ws)
            A.DoubleFP    -> A.F.Double <$> peek (castPtr ws)
            A.FP128FP     -> A.F.Quadruple <$> peekByteOff (castPtr ws) 8 <*> peekByteOff (castPtr ws) 0
            A.X86_FP80FP  -> A.F.X86_FP80 <$> peekByteOff (castPtr ws) 8 <*> peekByteOff (castPtr ws) 0
            A.PPC_FP128FP -> A.F.PPC_FP128 <$> peekByteOff (castPtr ws) 8 <*> peekByteOff (castPtr ws) 0
          )
      [valueSubclassIdP|ConstantPointerNull|] -> return $ A.C.Null t
      [valueSubclassIdP|ConstantAggregateZero|] -> return $ A.C.AggregateZero t
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
      [valueSubclassIdP|ConstantVector|] ->
            A.C.Vector <$> getConstantOperands
      [valueSubclassIdP|ConstantExpr|] -> do
            cppOpcode <- liftIO $ FFI.getConstantCPPOpcode c
            $(
              TH.caseE [| cppOpcode |] $
                (do (_, ((TH.RecC n fs, _), iDef)) <- Map.toList $
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
                                   | h == ''A.Constant &&
                                     TH.nameBase fn == "indices" -> do
                                       operandNumber <- get
                                       return [| mapM op [$(TH.litE . TH.integerL $ operandNumber)..nOps-1] |]

                                 _ -> error $ "unhandled constant expr field type: " ++ show fn ++ " - " ++ show ct
                          return [| $(o) `ap` $(a) |]
                    return $ TH.match
                              (TH.dataToPatQ (const Nothing) (ID.cppOpcode iDef))
                              (TH.normalB (evalState (foldM apWrapper [| return $(TH.conE n) |] fs) 0))
                              [])
                ++ [TH.match TH.wildP (TH.normalB [|error ("Unknown constant opcode: " <> show cppOpcode)|]) []]
             )
      [valueSubclassIdP|ConstantTokenNone|] -> return A.C.TokenNone
      _ -> error $ "unhandled constant valueSubclassId: " ++ show valueSubclassId




