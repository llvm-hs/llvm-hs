{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module LLVM.Test.ParameterAttribute where

import Test.Tasty
import Test.Tasty.QuickCheck                              hiding ( (.&.) )

import LLVM.Test.Support

import LLVM.AST.ParameterAttribute
import LLVM.Internal.Coding
import LLVM.Internal.Context
import LLVM.Internal.EncodeAST
import LLVM.Internal.DecodeAST
import qualified LLVM.Internal.FFI.Attribute              as FFI

import Control.Applicative
import Data.Bits
import Data.List
import Data.Word
import Text.Show.Pretty
import Control.Monad.IO.Class (liftIO)
import Prelude
import qualified Data.ByteString.Short                    as B


instance Arbitrary ParameterAttribute where
  arbitrary = oneof
    [ return ZeroExt
    , return SignExt
    , return InReg
    , return SRet
    , Alignment <$> elements (map (2^) [0..30 :: Int])
    , return NoAlias
    , return ByVal
    , return NoCapture
    , return NoFree
    , return Nest
    , return ReadNone
    , return ReadOnly
    , return WriteOnly
    , return ImmArg
    , return InAlloca
    , return NonNull
    , Dereferenceable <$> suchThat arbitrary (/= 0)
    , DereferenceableOrNull <$> suchThat arbitrary (/= 0)
    , return Returned
    , return SwiftSelf
    , return SwiftError
    , StringAttribute <$> arbitrarySbs <*> arbitrarySbs
    ]


tests :: TestTree
tests =
  testGroup "ParameterAttribute"
    [ testProperty "round-trip"  $ \attr ->
        ioProperty $ withContext $ \ctx  -> do
          attr' <- runEncodeAST ctx $ do
            attrSet <- encodeM [attr] :: EncodeAST FFI.ParameterAttributeSet
            liftIO (runDecodeAST (decodeM attrSet :: DecodeAST [ParameterAttribute]))
          return $ counterexample (unlines [ "expected: " ++ ppShow [attr]
                                           , "but got:  " ++ ppShow attr'
                                           ])
                                  ([attr] == attr')
    ]
