{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module LLVM.Test.FunctionAttribute where

import Test.Tasty
import Test.Tasty.QuickCheck                              hiding ( (.&.) )

import LLVM.Test.Support

import LLVM.AST.FunctionAttribute
import LLVM.Internal.Coding
import LLVM.Internal.Context
import LLVM.Internal.EncodeAST
import LLVM.Internal.DecodeAST
import qualified LLVM.Internal.FFI.Attribute              as FFI

import Control.Applicative
import Data.Bits
import Data.List
import Text.Show.Pretty
import Control.Monad.IO.Class (liftIO)
import Prelude
import qualified Data.ByteString.Short                    as B


instance Arbitrary FunctionAttribute where
  arbitrary = oneof
    [ return NoReturn
    , return NoUnwind
    , return ReadNone
    , return ReadOnly
    , return NoInline
    , return NoRecurse
    , return AlwaysInline
    , return MinimizeSize
    , return OptimizeForSize
    , return OptimizeNone
    , return StackProtect
    , return StackProtectReq
    , return StackProtectStrong
    , return StrictFP
    , return NoRedZone
    , return NoImplicitFloat
    , return Naked
    , return InlineHint
    , StackAlignment <$> elements (map (2^) [0..8 :: Int])
    , return ReturnsTwice
    , return UWTable
    , return NonLazyBind
    , return Builtin
    , return NoBuiltin
    , return Cold
    , return JumpTable
    , return NoDuplicate
    , return NoFree
    , return SanitizeAddress
    , return SanitizeHWAddress
    , return SanitizeThread
    , return SanitizeMemory
    , StringAttribute <$> (B.pack <$> arbitrary) <*> (B.pack <$> arbitrary)
    , suchThat (AllocSize <$> arbitrary <*> arbitrary) (/= AllocSize 0 (Just 0))
    , return WriteOnly
    , return ArgMemOnly
    , return Convergent
    , return InaccessibleMemOnly
    , return InaccessibleMemOrArgMemOnly
    , return SafeStack
    , return Speculatable
    ]

  shrink = \case
    StackAlignment x    -> map StackAlignment (nub [ v | u <- shrink x, let v = ceilPow2 u, v /= x ])
    StringAttribute x y -> [ StringAttribute (B.pack x') y | x' <- shrink (B.unpack x) ]
                        ++ [ StringAttribute x (B.pack y') | y' <- shrink (B.unpack y) ]
    AllocSize x y       -> [ AllocSize x' y | x' <- shrink x, not (x' == 0 && y == Just 0) ]
                        ++ [ AllocSize x y' | y' <- shrink y, not (x == 0 && y' == Just 0) ]
    _                   -> []


tests :: TestTree
tests =
  testGroup "FunctionAttribute"
    [ testProperty "round-trip"  $ \attr ->
        ioProperty $ withContext $ \ctx  -> do
          attr' <- runEncodeAST ctx $ do
            attrSet <- encodeM [attr] :: EncodeAST FFI.FunctionAttributeSet
            liftIO (runDecodeAST (decodeM attrSet :: DecodeAST [FunctionAttribute]))
          return $ counterexample (unlines [ "expected: " ++ ppShow [attr]
                                           , "but got:  " ++ ppShow attr'
                                           ])
                                  ([attr] == attr')
    ]


isPow2 :: (Bits a, Num a) => a -> Bool
isPow2 0 = True
isPow2 1 = False
isPow2 n = n .&. (n - 1) == 0

ceilPow2 :: (Bits a, Integral a) => a -> a
ceilPow2 n
  | isPow2 n  = n
  | otherwise =
      let x = logBase 2 (fromIntegral n) :: Double
          y = floor x + 1
      in
      1 `shiftL` y

