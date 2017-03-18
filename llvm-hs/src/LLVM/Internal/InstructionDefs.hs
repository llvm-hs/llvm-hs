{-# LANGUAGE
  TemplateHaskell,
  CPP
  #-}
module LLVM.Internal.InstructionDefs (
  astInstructionRecs,
  astConstantRecs,
  instructionDefs,
  ID.InstructionKind(..),
  ID.InstructionDef(..),
  instrP,
  innerJoin,
  outerJoin
  ) where

import LLVM.Prelude

import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Quote as TH

import Data.Map (Map)
import qualified Data.Map as Map

import qualified LLVM.Internal.FFI.InstructionDefs as ID

import qualified LLVM.AST.Instruction as A
import qualified LLVM.AST.Constant as A.C

$(do
   let ctorRecs t = do
#if __GLASGOW_HASKELL__ < 800
         TH.TyConI (TH.DataD _ _ _ cons _) <- TH.reify t
#else
         TH.TyConI (TH.DataD _ _ _ _ cons _) <- TH.reify t
#endif
         TH.dataToExpQ (const Nothing) $ [ (TH.nameBase n, rec) | rec@(TH.RecC n _) <- cons ]

   [d|
      astInstructionRecs :: Map String TH.Con
      astInstructionRecs = Map.fromList $(ctorRecs ''A.Instruction)
      astConstantRecs :: Map String TH.Con
      astConstantRecs = Map.fromList $(ctorRecs ''A.C.Constant)
    |]
 )

instructionDefs :: Map String ID.InstructionDef
instructionDefs = Map.fromList [ ((refName . ID.cAPIName $ i), i) | i <- ID.instructionDefs ]
  where
    refName "AtomicCmpXchg" = "CmpXchg"
    refName "PHI" = "Phi"
    refName x = x

innerJoin :: Ord k => Map k a -> Map k b -> Map k (a,b)
innerJoin = Map.intersectionWith (,)

outerJoin :: Ord k => Map k a -> Map k b -> Map k (Maybe a, Maybe b)
outerJoin xs ys = Map.unionWith combine
                  (Map.map (\a -> (Just a, Nothing)) xs)
                  (Map.map (\b -> (Nothing, Just b)) ys)
    where
      combine (Just a, Nothing) (Nothing, Just b) = (Just a, Just b)
      combine _ _ = error "outerJoin: the impossible happened"

instrP :: TH.QuasiQuoter
instrP = TH.QuasiQuoter {
  TH.quoteExp = undefined,
  TH.quotePat = let m = Map.fromList [ (ID.cAPIName i, ID.cppOpcode i) | i <- ID.instructionDefs ]
             in TH.dataToPatQ (const Nothing) . (m Map.!),
  TH.quoteType = undefined,
  TH.quoteDec = undefined
 }
