{-# LANGUAGE
  TemplateHaskell,
  QuasiQuotes,
  MultiParamTypeClasses,
  UndecidableInstances,
  ViewPatterns
  #-}
module LLVM.Internal.Instruction where

import LLVM.Prelude

import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Quote as TH
import qualified LLVM.Internal.InstructionDefs as ID
import LLVM.Internal.InstructionDefs (instrP)

import Control.Monad.AnyCont
import Control.Monad.IO.Class
import Control.Monad.State (gets)

import Foreign.Ptr

import Control.Exception (assert)
import Control.Monad.Catch
import qualified Data.Map as Map
import qualified Data.List as List
import Data.List.NonEmpty (NonEmpty((:|)))
import qualified Data.List.NonEmpty as NonEmpty

import qualified LLVM.Internal.FFI.Attribute as FFI
import qualified LLVM.Internal.FFI.PtrHierarchy as FFI
import qualified LLVM.Internal.FFI.BinaryOperator as FFI
import qualified LLVM.Internal.FFI.Instruction as FFI
import qualified LLVM.Internal.FFI.Value as FFI
import qualified LLVM.Internal.FFI.User as FFI
import qualified LLVM.Internal.FFI.Builder as FFI
import qualified LLVM.Internal.FFI.Constant as FFI
import qualified LLVM.Internal.FFI.BasicBlock as FFI

import LLVM.Internal.Atomicity ()
import LLVM.Internal.Attribute
import LLVM.Internal.CallingConvention ()
import LLVM.Internal.Coding
import LLVM.Internal.DecodeAST
import LLVM.Internal.EncodeAST
import LLVM.Internal.FastMathFlags ()
import LLVM.Internal.Metadata ()
import LLVM.Internal.Operand ()
import LLVM.Internal.RMWOperation ()
import LLVM.Internal.TailCallKind ()
import LLVM.Internal.Type
import LLVM.Internal.Value

import qualified LLVM.AST as A
import qualified LLVM.AST.Constant as A.C
import LLVM.Exception

callInstAttributeList :: Ptr FFI.Instruction -> DecodeAST AttributeList
callInstAttributeList instr =
  decodeM
    ( FFI.AttrSetDecoder
        FFI.getCallSiteAttributesAtIndex
        FFI.getCallSiteNumArgOperands
    , instr)

meta :: Ptr FFI.Instruction -> DecodeAST A.InstructionMetadata
meta i = do
  let getMetadata n = scopeAnyCont $ do
         ks <- allocaArray n
         ps <- allocaArray n
         n' <- liftIO $ FFI.getMetadata i ks ps n
         if (n' > n)
          then getMetadata n'
          else return zip `ap` decodeM (n', ks) `ap` decodeM (n', ps)
  getMetadata 4

setMD :: Ptr FFI.Instruction -> A.InstructionMetadata -> EncodeAST ()
setMD i md = forM_ md $ \(kindName, anode) -> do
               kindID <- encodeM kindName
               node <- encodeM anode
               liftIO $ FFI.setMetadata i kindID node

instance DecodeM DecodeAST A.Terminator (Ptr FFI.Instruction) where
  decodeM i = scopeAnyCont $ do
    n <- liftIO $ FFI.getInstructionDefOpcode i
    nOps <- liftIO $ FFI.getNumOperands (FFI.upCast i)
    md <- meta i
    let op n = decodeM =<< (liftIO $ FFI.getOperand (FFI.upCast i) n)
        successor n = decodeM =<< (liftIO $ FFI.isABasicBlock =<< FFI.getOperand (FFI.upCast i) n)
    case n of
      [instrP|Ret|] -> do
        returnOperand' <- if nOps == 0 then return Nothing else Just <$> op 0
        return $ A.Ret { A.returnOperand = returnOperand', A.metadata' = md }
      [instrP|Br|] -> do
        n <- liftIO $ FFI.getNumOperands (FFI.upCast i)
        case n of
          1 -> do
             dest <- successor 0
             return $ A.Br { A.dest = dest, A.metadata' = md }
          3 -> do
             condition <- op 0
             falseDest <- successor 1
             trueDest <- successor 2
             return $ A.CondBr {
               A.condition = condition,
               A.falseDest = falseDest,
               A.trueDest = trueDest,
               A.metadata' = md
             }
          _ -> error "Branch instructions should always have 1 or 3 operands"
      [instrP|Switch|] -> do
        op0 <- op 0
        dd <- successor 1
        let nCases = (nOps - 2) `div` 2
        values <- allocaArray nCases
        dests <- allocaArray nCases
        liftIO $ FFI.getSwitchCases i values dests
        cases <- return zip `ap` peekArray nCases values `ap` peekArray nCases dests
        dests <- forM cases $ \(c, d) -> return (,) `ap` decodeM c `ap` decodeM d
        return A.Switch {
          A.operand0' = op0,
          A.defaultDest = dd,
          A.dests = dests,
          A.metadata' = md
        }
      [instrP|IndirectBr|] -> do
        op0 <- op 0
        let nDests = nOps - 1
        dests <- allocaArray nDests
        liftIO $ FFI.getIndirectBrDests i dests
        dests <- decodeM (nDests, dests)
        return A.IndirectBr {
           A.operand0' = op0,
           A.possibleDests = dests,
           A.metadata' = md
        }
      [instrP|Invoke|] -> do
        cc <- decodeM =<< liftIO (FFI.getCallSiteCallingConvention i)
        attrs <- callInstAttributeList i
        fv <- liftIO $ FFI.getCallSiteCalledValue i
        f <- decodeM fv
        let argIndices = if nOps >= 4 then [0 .. nOps - 4] else []
        args <-
          forM (leftBiasedZip argIndices (parameterAttributes attrs)) $ \(j, pAttrs) ->
                  (, fromMaybe [] pAttrs) <$> op j
        rd <- successor (nOps - 3)
        ed <- successor (nOps - 2)
        return A.Invoke {
          A.callingConvention' = cc,
          A.returnAttributes' = returnAttributes attrs,
          A.function' = f,
          A.arguments' = args,
          A.functionAttributes' = functionAttributes attrs,
          A.returnDest = rd,
          A.exceptionDest = ed,
          A.metadata' = md
        }
      [instrP|Resume|] -> do
        op0 <- op 0
        return A.Resume {
          A.operand0' = op0,
          A.metadata' = md
        }
      [instrP|Unreachable|] -> do
        return A.Unreachable {
          A.metadata' = md
        }
      [instrP|CleanupRet|] -> do
        dest <- decodeM =<< liftIO (FFI.upCast <$> (FFI.getCleanupPad i) :: IO (Ptr FFI.Value))
        unwindDest <- decodeM =<< liftIO (FFI.getUnwindDest i)
        return A.CleanupRet {
          A.cleanupPad = dest,
          A.unwindDest = unwindDest,
          A.metadata' = md
        }
      [instrP|CatchRet|] -> do
        catchPad <- decodeM =<< liftIO (FFI.catchRetGetCatchPad i)
        successor <- decodeM =<< liftIO (FFI.catchRetGetSuccessor i)
        return A.CatchRet {
          A.catchPad = catchPad,
          A.successor = successor,
          A.metadata' = md
        }
      [instrP|CatchSwitch|] -> do
        parentPad' <- decodeM =<< liftIO (FFI.catchSwitchGetParentPad i)
        numHandlers <- liftIO (FFI.catchSwitchGetNumHandlers i)
        handlers <- assert (numHandlers > 0) $
          forM (0 :| [1..numHandlers - 1]) $ decodeM <=< liftIO . FFI.catchSwitchGetHandler i
        unwindDest <- decodeM =<< liftIO (FFI.catchSwitchGetUnwindDest i)
        return A.CatchSwitch {
          A.parentPad' = parentPad',
          A.catchHandlers = handlers,
          A.defaultUnwindDest = unwindDest,
          A.metadata' = md
        }
      i -> error ("Unknown terminator instruction kind: " <> show i)

instance EncodeM EncodeAST A.Terminator (Ptr FFI.Instruction) where
  encodeM t = scopeAnyCont $ do
    builder <- gets encodeStateBuilder
    s <- encodeM ""
    t' <- case t of
      A.Ret { A.returnOperand = r } -> do
        rv <- maybe (return nullPtr) encodeM r
        FFI.upCast <$> do liftIO $ FFI.buildRet builder rv
      A.Br { A.dest = d } -> do
        db <- encodeM d
        FFI.upCast <$> do liftIO $ FFI.buildBr builder db
      A.CondBr { A.condition = c, A.trueDest = t, A.falseDest = f } -> do
        cv <- encodeM c
        tb <- encodeM t
        fb <- encodeM f
        FFI.upCast <$> do liftIO $ FFI.buildCondBr builder cv tb fb
      A.Switch {
        A.operand0' = op0,
        A.defaultDest = dd,
        A.dests = ds
      } -> do
        op0' <- encodeM op0
        dd' <- encodeM dd
        i <- liftIO $ FFI.buildSwitch builder op0' dd' (fromIntegral $ length ds)
        forM_ ds $ \(v,d) -> do
          v' <- encodeM v
          d' <- encodeM d
          liftIO $ FFI.addCase i v' d'
        return $ FFI.upCast i
      A.IndirectBr {
        A.operand0' = op0,
        A.possibleDests = dests
      } -> do
        op0' <- encodeM op0
        i <- liftIO $ FFI.buildIndirectBr builder op0' (fromIntegral $ length dests)
        forM_ dests $ \dest -> do
          d <- encodeM dest
          liftIO $ FFI.addDestination i d
        return $ FFI.upCast i
      A.Invoke {
        A.callingConvention' = cc,
        A.returnAttributes' = rAttrs,
        A.function' = fun,
        A.arguments' = args,
        A.functionAttributes' = fAttrs,
        A.returnDest = rd,
        A.exceptionDest = ed
      } -> do
        fv <- encodeM fun
        rb <- encodeM rd
        eb <- encodeM ed
        let (argvs, argAttrs) = unzip args
        (n, argvs) <- encodeM argvs
        i <- liftIO $ FFI.buildInvoke builder fv argvs n rb eb s
        attrs <- encodeM $ AttributeList fAttrs rAttrs argAttrs
        liftIO $ FFI.setCallSiteAttributeList i attrs
        cc <- encodeM cc
        liftIO $ FFI.setCallSiteCallingConvention i cc
        return $ FFI.upCast i
      A.Resume {
        A.operand0' = op0
      } -> do
        op0' <- encodeM op0
        i <- liftIO $ FFI.buildResume builder op0'
        return $ FFI.upCast i
      A.Unreachable {
      } -> do
        i <- liftIO $ FFI.buildUnreachable builder
        return $ FFI.upCast i
      A.CleanupRet {
        A.cleanupPad = cleanupPad,
        A.unwindDest = unwindDest
      } -> do
        cleanupPad' <- encodeM cleanupPad
        unwindDest' <- encodeM unwindDest
        liftIO $ FFI.buildCleanupRet builder cleanupPad' unwindDest'
      A.CatchRet {
        A.catchPad = catchPad,
        A.successor = successor
      } -> do
        catchPad' <- encodeM catchPad
        successor' <- encodeM successor
        liftIO $ FFI.buildCatchRet builder catchPad' successor'
      A.CatchSwitch {
        A.parentPad' = parentPad,
        A.catchHandlers = catchHandlers,
        A.defaultUnwindDest = unwindDest
      } -> do
        parentPad' <- encodeM parentPad
        unwindDest' <- encodeM unwindDest
        let numHandlers = fromIntegral (NonEmpty.length catchHandlers)
        i <- liftIO $ FFI.buildCatchSwitch builder parentPad' unwindDest' numHandlers
        mapM_ (liftIO . FFI.catchSwitchAddHandler i <=< encodeM) catchHandlers
        return i
    setMD t' (A.metadata' t)
    return t'

$(do
  let findInstrFields s = Map.findWithDefault (error $ "instruction missing from AST: " ++ show s) s
                          ID.astInstructionRecs

  [d|
    instance DecodeM DecodeAST A.Instruction (Ptr FFI.Instruction) where
      decodeM i = scopeAnyCont $ do
        t <- typeOf i
        nOps <- liftIO $ FFI.getNumOperands (FFI.upCast i)
        let op n = decodeM =<< (liftIO $ FFI.getOperand (FFI.upCast i) n)
            cop n = decodeM =<< (liftIO $ FFI.isAConstant =<< FFI.getOperand (FFI.upCast i) n)
            get_nsw b = liftIO $ decodeM =<< FFI.hasNoSignedWrap (FFI.upCast b)
            get_nuw b = liftIO $ decodeM =<< FFI.hasNoUnsignedWrap (FFI.upCast b)
            get_exact b = liftIO $ decodeM =<< FFI.isExact (FFI.upCast b)
            get_fastMathFlags b = liftIO $ decodeM =<< FFI.getFastMathFlags (FFI.upCast b)

        n <- liftIO $ FFI.getInstructionDefOpcode i
        $(
          let fieldDecoders :: String -> String -> ([String], TH.ExpQ)
              fieldDecoders lrn s = case s of
                "b" -> ([], [| liftIO $ FFI.isABinaryOperator (FFI.upCast i) |])
                "nsw" -> (["b"], [| get_nsw $(TH.dyn "b") |])
                "nuw" -> (["b"], [| get_nuw $(TH.dyn "b") |])
                "exact" -> (["b"], [| get_exact $(TH.dyn "b") |])
                "fastMathFlags" -> (["b"], [| get_fastMathFlags $(TH.dyn "b") |])
                "operand0" -> ([], [| op 0 |])
                "operand1" -> ([], [| op 1 |])
                "address" -> ([], case lrn of "Store" -> [| op 1 |]; _ -> [| op 0 |])
                "value" -> ([], case lrn of "Store" -> [| op 0 |]; _ -> [| op 1 |])
                "expected" -> ([], [| op 1 |])
                "replacement" -> ([], [| op 2 |])
                "condition'" -> ([], [| op 0 |])
                "trueValue" -> ([], [| op 1 |])
                "falseValue" -> ([], [| op 2 |])
                "argList" -> ([], [| op 0 |])
                "vector" -> ([], [| op 0 |])
                "element" -> ([], [| op 1 |])
                "index" -> ([], case lrn of
                                  "ExtractElement" -> [| op 1 |]
                                  "InsertElement" -> [| op 2 |]
                                  _ -> [|error "Index fields are only supported for 'ExtractElement' and 'InsertElement': " <> lrn|])
                "mask" -> ([], [| cop 2 |])
                "aggregate" -> ([], [| op 0 |])
                "metadata" -> ([], [| meta i |])
                "iPredicate" -> ([], [| decodeM =<< liftIO (FFI.getICmpPredicate i) |])
                "fpPredicate" -> ([], [| decodeM =<< liftIO (FFI.getFCmpPredicate i) |])
                "tailCallKind" -> ([], [| decodeM =<< liftIO (FFI.getTailCallKind i) |])
                "callingConvention" -> ([], [| decodeM =<< liftIO (FFI.getCallSiteCallingConvention i) |])
                "attrs" -> ([], [| callInstAttributeList i |])
                "returnAttributes" -> (["attrs"], [| return $ returnAttributes $(TH.dyn "attrs") |])
                "f" -> ([], [| liftIO $ FFI.getCallSiteCalledValue i |])
                "function" -> (["f"], [| decodeM $(TH.dyn "f") |])
                "arguments" -> ([], [| forM (leftBiasedZip [1..nOps-1] (parameterAttributes $(TH.dyn "attrs"))) $ \(j, pAttrs) ->
                                         (\p -> (p, fromMaybe [] pAttrs)) <$> op (j - 1) |])
                "clauses" ->
                  ([], [|do
                          nClauses <- liftIO $ FFI.getNumClauses i
                          -- We need to convert nClauses to a signed
                          -- value before subtracting
                          forM [0..fromIntegral nClauses - (1 :: Int)] $ \j -> do
                            v <- liftIO $ FFI.getClause i (fromIntegral j)
                            c <- decodeM v
                            t <- typeOf v
                            return $ case t of { A.ArrayType _ _ -> A.Filter; _ -> A.Catch} $ c |])
                "functionAttributes" -> (["attrs"], [| return $ functionAttributes $(TH.dyn "attrs") |])
                "type'" -> ([], [| return t |])
                "incomingValues" ->
                    ([], [| do
                            n <- liftIO $ FFI.countIncoming i
                            forM [0..n-1] $ \m -> do
                              iv <- decodeM =<< (liftIO $ FFI.getIncomingValue i m)
                              ib <- decodeM =<< (liftIO $ FFI.getIncomingBlock i m)
                              return (iv,ib) |])
                "allocatedType" -> ([], [| decodeM =<< liftIO (FFI.getAllocatedType i) |])
                "numElements" ->
                    ([], [| do
                            n <- decodeM =<< (liftIO $ FFI.getAllocaNumElements i)
                            return $ case n of
                              A.ConstantOperand (A.C.Int { A.C.integerValue = 1 }) -> Nothing
                              _ -> Just n
                              |])
                "alignment" -> ([], [| decodeM =<< liftIO (FFI.getInstrAlignment i) |])
                "maybeAtomicity" -> ([], [| decodeM =<< liftIO (FFI.getAtomicity i) |])
                "atomicity" -> ([], [| decodeM =<< liftIO (FFI.getAtomicity i) |])
                "volatile" -> ([], [| decodeM =<< liftIO (FFI.getVolatile i) |])
                "inBounds" -> ([], [| decodeM =<< liftIO (FFI.getInBounds (FFI.upCast i)) |])
                "failureMemoryOrdering" -> ([], [| decodeM =<< liftIO (FFI.getFailureAtomicOrdering i) |])
                "indices" -> ([], [| mapM op [1..nOps-1] |])
                "indices'" ->
                  ([], [| do
                          n <- liftIO $ FFI.countInstStructureIndices i
                          a <- allocaArray n
                          liftIO $ FFI.getInstStructureIndices i a
                          decodeM (n, a) |])
                "rmwOperation" -> ([], [| decodeM =<< liftIO (FFI.getAtomicRMWBinOp i) |])
                "cleanup" -> ([], [| decodeM =<< liftIO (FFI.isCleanup i) |])
                "parentPad" -> ([], [| decodeM =<< liftIO (FFI.getParentPad i) |])
                "catchSwitch" -> ([], [| decodeM =<< liftIO (FFI.getParentPad i) |])
                "args" -> ([], [| do numArgs <- liftIO (FFI.getNumArgOperands i)
                                     if (numArgs == 0)
                                       then return []
                                       else forM [0..numArgs-1] $ \op ->
                                              decodeM =<< liftIO (FFI.getArgOperand i op) |])
                _ -> ([], [| error $ "unrecognized instruction field or depenency thereof: " ++ show s |])
          in
          TH.caseE [| n |] $
            [ TH.match opcodeP (TH.normalB (TH.doE handlerBody)) []
            | (lrn, iDef) <- Map.toList ID.instructionDefs,
              ID.instructionKind iDef /= ID.Terminator,
              let opcodeP = TH.dataToPatQ (const Nothing) (ID.cppOpcode iDef)
                  handlerBody =
                    let TH.RecC fullName fields = findInstrFields lrn
                        (fieldNames,_,_) = unzip3 fields
                        allNames ns = List.nub $ [ d | n <- ns, d <- allNames . fst . fieldDecoders lrn $ n ] ++ ns
                    in
                      [
                       TH.bindS (TH.varP (TH.mkName n)) (snd . fieldDecoders lrn $ n)
                       | n <- allNames . map TH.nameBase $ fieldNames
                      ] ++ [
                       TH.noBindS [|
                        return $(TH.recConE
                                 fullName
                                 [ (f,) <$> (TH.varE . TH.mkName . TH.nameBase $ f) | f <- fieldNames ])
                        |]
                      ]
            ] ++
            [ TH.match TH.wildP (TH.normalB [| error ("Unknown instruction opcode: " <> show n) |]) [] ]
         )

    instance EncodeM EncodeAST A.Instruction (Ptr FFI.Instruction, EncodeAST ()) where
      encodeM o = scopeAnyCont $ do
        builder <- gets encodeStateBuilder
        let return' i = return (FFI.upCast i, return ())
        s <- encodeM ""
        (inst, act) <- case o of
          A.ICmp {
            A.iPredicate = pred,
            A.operand0 = op0,
            A.operand1 = op1
          } -> do
            op0' <- encodeM op0
            op1' <- encodeM op1
            pred <- encodeM pred
            i <- liftIO $ FFI.buildICmp builder pred op0' op1' s
            return' i
          A.FCmp {
            A.fpPredicate = pred,
            A.operand0 = op0,
            A.operand1 = op1
          } -> do
            op0' <- encodeM op0
            op1' <- encodeM op1
            pred <- encodeM pred
            i <- liftIO $ FFI.buildFCmp builder pred op0' op1' s
            return' i
          A.Phi { A.type' = t, A.incomingValues = ivs } -> do
             t' <- encodeM t
             i <- liftIO $ FFI.buildPhi builder t' s
             return (
               FFI.upCast i,
               do
                 let (ivs3, bs3) = unzip ivs
                 ivs3' <- encodeM ivs3
                 bs3' <- encodeM bs3
                 liftIO $ FFI.addIncoming i ivs3' bs3'
               )
          A.Call {
            A.tailCallKind = tck,
            A.callingConvention = cc,
            A.returnAttributes = rAttrs,
            A.function = f,
            A.arguments = args,
            A.functionAttributes = fAttrs
          } -> do
            fv <- encodeM f
            let (argvs, argAttrs) = unzip args
            (n, argvs) <- encodeM argvs
            i <- liftIO $ FFI.buildCall builder fv argvs n s
            attrs <- encodeM $ AttributeList fAttrs rAttrs argAttrs
            liftIO $ FFI.setCallSiteAttributeList i attrs
            tck <- encodeM tck
            liftIO $ FFI.setTailCallKind i tck
            cc <- encodeM cc
            liftIO $ FFI.setCallSiteCallingConvention i cc
            return' i
          A.Select { A.condition' = c, A.trueValue = t, A.falseValue = f } -> do
            c' <- encodeM c
            t' <- encodeM t
            f' <- encodeM f
            i <- liftIO $ FFI.buildSelect builder c' t' f' s
            return' i
          A.VAArg { A.argList = al, A.type' = t } -> do
            al' <- encodeM al
            t' <- encodeM t
            i <- liftIO $ FFI.buildVAArg builder al' t' s
            return' i
          A.ExtractElement { A.vector = v, A.index = idx } -> do
            v' <- encodeM v
            idx' <- encodeM idx
            i <- liftIO $ FFI.buildExtractElement builder v' idx' s
            return' i
          A.InsertElement { A.vector = v, A.element = e, A.index = idx } -> do
            v' <- encodeM v
            e' <- encodeM e
            idx' <- encodeM idx
            i <- liftIO $ FFI.buildInsertElement builder v' e' idx' s
            return' i
          A.ShuffleVector { A.operand0 = o0, A.operand1 = o1, A.mask = mask } -> do
            o0' <- encodeM o0
            o1' <- encodeM o1
            mask' <- encodeM mask
            i <- liftIO $ FFI.buildShuffleVector builder o0' o1' mask' s
            return' i
          A.ExtractValue { A.aggregate = a, A.indices' = is } -> do
            a' <- encodeM a
            (n, is') <- encodeM is
            i <- liftIO $ FFI.buildExtractValue builder a' is' n s
            return' i
          A.InsertValue { A.aggregate = a, A.element = e, A.indices' = is } -> do
            a' <- encodeM a
            e' <- encodeM e
            (n, is') <- encodeM is
            i <- liftIO $ FFI.buildInsertValue builder a' e' is' n s
            return' i
          A.LandingPad {
            A.type' = t,
            A.cleanup = cl,
            A.clauses = cs
          } -> do
            t' <- encodeM t
            i <- liftIO $ FFI.buildLandingPad builder t' (fromIntegral $ length cs) s
            forM_ cs $ \c ->
              case c of
                A.Catch a -> do
                  cn <- encodeM a
                  isArray <- liftIO $ isArrayType =<< FFI.typeOf (FFI.upCast cn)
                  when isArray $ throwM . EncodeException $ "Catch clause cannot take an array: " ++ show c
                  liftIO $ FFI.addClause i cn
                A.Filter a -> do
                  cn <- encodeM a
                  isArray <- liftIO $ isArrayType =<< FFI.typeOf (FFI.upCast cn)
                  unless isArray $ throwM . EncodeException $ "filter clause must take an array: " ++ show c
                  liftIO $ FFI.addClause i cn
            when cl $ do
              cl <- encodeM cl
              liftIO $ FFI.setCleanup i cl
            return' i
          A.Alloca { A.allocatedType = alt, A.numElements = n, A.alignment = alignment } -> do
             alt' <- encodeM alt
             n' <- encodeM n
             i <- liftIO $ FFI.buildAlloca builder alt' n' s
             unless (alignment == 0) $ liftIO $ FFI.setInstrAlignment i (fromIntegral alignment)
             return' i
          A.CleanupPad { A.parentPad = parentPad, A.args = args } -> do
            parentPad' <- encodeM parentPad
            (numArgs, args') <- encodeM args
            i <- liftIO $ FFI.buildCleanupPad builder parentPad' args' numArgs s
            return' i
          A.CatchPad { A.catchSwitch = catchSwitch, A.args = args } -> do
            catchSwitch' <- encodeM catchSwitch
            (numArgs, args') <- encodeM args
            i <- liftIO $ FFI.buildCatchPad builder catchSwitch' args' numArgs s
            return' i
          o -> $(TH.caseE [| o |] $
                  [TH.match
                   (TH.recP fullName [ (f,) <$> (TH.varP . TH.mkName . TH.nameBase $ f) | f <- encodeFieldNames ])
                   (TH.normalB (TH.doE handlerBody))
                   []
                   |
                   (name, ID.instructionKind -> k) <- Map.toList ID.instructionDefs,
                   case (k, name) of
                     (ID.Binary, _) -> True
                     (ID.Cast, _) -> True
                     (ID.Memory, "Alloca") -> False
                     (ID.Memory, _) -> True
                     _ -> False,
                   let
                     TH.RecC fullName (unzip3 -> (fieldNames, _, _)) = findInstrFields name
                     encodeFieldNames = filter (\f -> TH.nameBase f /= "metadata") fieldNames
                     encodeMFields = map TH.nameBase encodeFieldNames
                     handlerBody = ([
                       TH.bindS (if s == "fastMathFlags" then TH.tupP [] else TH.varP (TH.mkName s))
                           [| encodeM $(TH.dyn s) |] | s <- encodeMFields
                      ] ++ [
                       TH.bindS (TH.varP (TH.mkName "i")) [| liftIO $ $(
                          foldl1 TH.appE . map TH.dyn $
                           [ "FFI.build" ++ name, "builder" ] ++ (encodeMFields List.\\ [ "fastMathFlags" ]) ++ [ "s" ]
                        ) |],
                       TH.noBindS [| return' $(TH.dyn "i") |]
                      ])
                  ] ++
                  (map (\p -> TH.match p (TH.normalB [|inconsistentCases "Instruction" o|]) [])
                       [[p|A.Alloca{}|],
                        [p|A.ICmp{}|],
                        [p|A.FCmp{}|],
                        [p|A.Phi{}|],
                        [p|A.Call{}|],
                        [p|A.Select{}|],
                        [p|A.VAArg{}|],
                        [p|A.ExtractElement{}|],
                        [p|A.InsertElement{}|],
                        [p|A.ShuffleVector{}|],
                        [p|A.ExtractValue{}|],
                        [p|A.InsertValue{}|],
                        [p|A.LandingPad{}|],
                        [p|A.CatchPad{}|],
                        [p|A.CleanupPad{}|]])
                )

        setMD inst (A.metadata o)
        return (inst, act)
   |]
 )


instance DecodeM DecodeAST a (Ptr FFI.Instruction) => DecodeM DecodeAST (DecodeAST (A.Named a)) (Ptr FFI.Instruction) where
  decodeM i = do
    t <- typeOf i
    w <- if t == A.VoidType then (return A.Do) else (return (A.:=) `ap` getLocalName i)
    return $ return w `ap` decodeM i

guardNonVoidType :: (MonadIO m, MonadThrow m) => Ptr FFI.Instruction -> String -> m ()
guardNonVoidType instr expr = do
  ty <- (liftIO . runDecodeAST . typeOf) instr
  case ty of
    A.VoidType -> throwM (EncodeException ("Instruction of type void must not have a name: " ++ expr))
    _ -> return ()

instance (EncodeM EncodeAST a (Ptr FFI.Instruction), Show a) => EncodeM EncodeAST (A.Named a) (Ptr FFI.Instruction) where
  encodeM (A.Do o) = encodeM o
  encodeM assgn@(n A.:= o) = do
    i <- encodeM o
    let v = FFI.upCast i
    n' <- encodeM n
    liftIO $ FFI.setValueName v n'
    defineLocal n v
    guardNonVoidType i (show assgn)
    return i

instance (EncodeM EncodeAST a (Ptr FFI.Instruction, EncodeAST ()), Show a) => EncodeM EncodeAST (A.Named a) (EncodeAST ()) where
  encodeM (A.Do o) = liftM snd $ (encodeM o :: EncodeAST (Ptr FFI.Instruction, EncodeAST ()))
  encodeM assgn@(n A.:= o) = do
    (i, later) <- encodeM o
    let v = FFI.upCast (i :: Ptr FFI.Instruction)
    n' <- encodeM n
    liftIO $ FFI.setValueName v n'
    defineLocal n v
    guardNonVoidType i (show assgn)
    return later
