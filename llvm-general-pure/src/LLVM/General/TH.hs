{-# LANGUAGE CPP #-}
-- | This module is presents template haskell mostly like the template-haskell >= 2.10 / ghc >= 7.10,
-- even on earlier versions. It's intended as an internal library for llvm-general-pure and llvm-general;
-- it's exposed only to be shared between the two.
module LLVM.General.TH (
    module Language.Haskell.TH,
    conT, appT
  ) where

#if __GLASGOW_HASKELL__ < 710
import LLVM.General.Prelude
#endif

import qualified Language.Haskell.TH as TH (conT, appT)
import Language.Haskell.TH hiding (conT, appT)

class Typish qt where
  appT :: qt -> Q Type -> qt
  conT :: Name -> qt

instance Typish (Q Type) where
  appT = TH.appT
  conT = TH.conT

#if __GLASGOW_HASKELL__ < 710
instance Typish (Q Pred) where
  appT qp qt = do
    ClassP n ts <- qp
    t <- qt
    return $ ClassP n (ts ++ [t])
  conT n = classP n []
#endif
