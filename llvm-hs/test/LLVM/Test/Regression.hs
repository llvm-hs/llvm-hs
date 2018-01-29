{-# LANGUAGE OverloadedStrings #-}
module LLVM.Test.Regression
  ( tests
  ) where

import           Test.Tasty
import           Test.Tasty.HUnit

import qualified LLVM.AST as AST
import           LLVM.AST hiding (Module)
import           LLVM.AST.Constant
import           LLVM.AST.Global
import           LLVM.AST.Type

import           LLVM.Context
import           LLVM.Exception
import           LLVM.Module

import           Control.Exception

example1 :: AST.Module
example1 =
  defaultModule
  { moduleDefinitions =
      [ GlobalDefinition
          functionDefaults
          { name = "test"
          , returnType = void
          , basicBlocks =
              [ BasicBlock
                  "entry"
                  [ UnName 0 := Alloca i32 Nothing 0 []
                  , UnName 1 :=
                    Store
                      False
                      (LocalReference (ptr i32) (UnName 0))
                      (ConstantOperand (Int 32 42))
                      Nothing
                      0
                      []
                  ]
                  (Do $ Ret Nothing [])
              ]
          }
      ]
  }

example2 :: AST.Module
example2 =
  defaultModule
  { moduleDefinitions =
      [ GlobalDefinition
          functionDefaults
          { name = "test"
          , returnType = void
          , basicBlocks =
              [ BasicBlock
                  "entry"
                  [ UnName 0 :=
                    Alloca (ptr $ FunctionType void [] False) Nothing 0 []
                  , Do $
                    Store
                      False
                      (LocalReference
                         (ptr $ ptr $ FunctionType void [] False)
                         (UnName 0))
                      (ConstantOperand $
                       GlobalReference (FunctionType void [] False) "test")
                      Nothing
                      0
                      []
                  ]
                  (Do $ Ret Nothing [])
              ]
          }
      ]
  }

shouldThrowEncodeException :: AST.Module -> String -> IO ()
shouldThrowEncodeException ast errMsg = do
  result <- try $ withContext $ \context -> do
    withModuleFromAST context ast (\_ -> return ())
  case result of
    Left (EncodeException actualErrMsg) -> actualErrMsg @?= errMsg
    Right _ -> assertFailure ("Expected serialization to fail with: \"" ++ errMsg ++ "\"")

tests :: TestTree
tests =
  testGroup
    "Regression"
    [ testCase
        "no named voids"
        (example1 `shouldThrowEncodeException`
         "Instruction of type void must not have a name: UnName 1 := Store {volatile = False, address = LocalReference (PointerType {pointerReferent = IntegerType {typeBits = 32}, pointerAddrSpace = AddrSpace 0}) (UnName 0), value = ConstantOperand (Int {integerBits = 32, integerValue = 42}), maybeAtomicity = Nothing, alignment = 0, metadata = []}")
    , testCase
        "no implicit casts"
        (example2 `shouldThrowEncodeException`
         "The serialized GlobalReference Name \"test\" has type FunctionType {resultType = VoidType, argumentTypes = [], isVarArg = False} but should have type PointerType {pointerReferent = FunctionType {resultType = VoidType, argumentTypes = [], isVarArg = False}, pointerAddrSpace = AddrSpace 0}")
    ]
