{-# LANGUAGE
  TemplateHaskell
  #-}
module LLVM.Internal.FFI.Cleanup where

import LLVM.Prelude

import Language.Haskell.TH
import Data.Sequence as Seq

import Foreign.C
import Foreign.Ptr

import LLVM.Internal.FFI.LLVMCTypes
import qualified LLVM.Internal.FFI.PtrHierarchy as FFI

import qualified LLVM.AST.IntegerPredicate as A (IntegerPredicate) 
import qualified LLVM.AST.FloatingPointPredicate as A (FloatingPointPredicate) 
import qualified LLVM.AST.Constant as A.C (Constant)
import qualified LLVM.AST.Operand as A (Operand)
import qualified LLVM.AST.Type as A (Type)
import qualified LLVM.AST.Instruction as A (FastMathFlags)

foreignDecl :: String -> String -> [TypeQ] -> TypeQ -> DecsQ
foreignDecl cName hName argTypeQs returnTypeQ = do
  let retTyQ = appT (conT ''IO) returnTypeQ
      foreignDecl' hName argTypeQs =
        forImpD cCall unsafe cName (mkName hName) 
                  (foldr (\a b -> appT (appT arrowT a) b) retTyQ argTypeQs)
      splitTuples :: [Type] -> Q ([Type], [Pat], [Exp])
      splitTuples ts = do
        let f :: Type -> Q (Seq Type, Pat, Seq Exp)
            f x@(AppT _ _) = maybe (d x) (\q -> q >>= \(ts, ps, es) -> return (ts, TupP (toList ps), es)) (g 0 x)
            f x = d x
            g :: Int -> Type -> Maybe (Q (Seq Type, Seq Pat, Seq Exp))
            g n (TupleT m) | m == n = return (return (Seq.empty, Seq.empty, Seq.empty))
            g n (AppT a b) = do
              k <- g (n+1) a
              return $ do
                (ts, ps, es) <- k
                (ts', p', es') <- f b
                return (ts >< ts', ps |> p', es >< es')
            g _ _ = Nothing
            d :: Type -> Q (Seq Type, Pat, Seq Exp)
            d x = do
              n <- newName "v"
              return (Seq.singleton x, VarP n, Seq.singleton (VarE n))
            seqsToList :: [Seq a] -> [a]
            seqsToList = toList . foldr (><) Seq.empty
                
        (tss, ps, ess) <- liftM unzip3 . mapM f $ ts
        return (seqsToList tss, ps, seqsToList ess)

                                
  argTypes <- sequence argTypeQs
  (ts, ps, es) <- splitTuples argTypes
  let phName = hName ++ "'"
  sequence [
    foreignDecl' phName (map return ts),
    sigD (mkName hName) (foldr (\argT retT -> appT (appT arrowT argT) retT) retTyQ argTypeQs),
    funD (mkName hName) [
     clause (map return ps) (normalB (foldl appE (varE (mkName phName)) (map return es))) []
    ]
   ]

-- | The LLVM C-API for instructions with boolean flags (e.g. nsw) and is weak, so they get
-- separated out for different handling. This check is an accurate but crude test for whether
-- an instruction needs such handling.
hasFlags :: [Type] -> Bool
hasFlags = any (== ConT ''Bool)

typeMapping :: Type -> TypeQ
typeMapping t = case t of
  ConT h | h == ''Bool -> [t| LLVMBool |]
         | h == ''Int32 -> [t| CInt |]
         | h == ''Word32 -> [t| CUInt |]
         | h == ''String -> [t| CString |]
         | h == ''A.Operand -> [t| Ptr FFI.Value |]
         | h == ''A.Type -> [t| Ptr FFI.Type |]
         | h == ''A.C.Constant -> [t| Ptr FFI.Constant |]
         | h == ''A.FloatingPointPredicate -> [t| FCmpPredicate |]
         | h == ''A.IntegerPredicate -> [t| ICmpPredicate |]
         | h == ''A.FastMathFlags -> [t| FastMathFlags |]
  AppT ListT x -> foldl1 appT [tupleT 2, [t| CUInt |], appT [t| Ptr |] (typeMapping x)]
  x -> error $ "type not handled in Cleanup typeMapping: " ++ show x
