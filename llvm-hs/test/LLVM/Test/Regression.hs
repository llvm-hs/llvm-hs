{-# LANGUAGE OverloadedStrings #-}
module LLVM.Test.Regression
  ( tests
  ) where

import           Test.Tasty
import           Test.Tasty.HUnit

import qualified LLVM.AST as AST
import           LLVM.AST hiding (Module)
import qualified LLVM.AST.Constant as C
import           LLVM.AST.Global hiding (metadata)
import           LLVM.AST.Type
import           LLVM.IRBuilder

import           LLVM.Context
import           LLVM.Exception
import           LLVM.Module

import           Data.ByteString as B (readFile)
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
                      (LocalReference ptr (UnName 0))
                      (ConstantOperand (C.Int 32 42))
                      Nothing
                      0
                      []
                  ]
                  (Do $ Ret Nothing [])
              ]
          }
      ]
  }

duplicateDefinitions :: AST.Module
duplicateDefinitions =
  defaultModule
    { moduleName = "clashingModule"
    , moduleDefinitions =
        [ GlobalDefinition
            functionDefaults
              { name = "clashy"
              , returnType = i64
              , basicBlocks =
                  [ BasicBlock
                      "entry"
                      [ mkName "c" :=
                        Add
                          False
                          False
                          (ConstantOperand (C.Int 64 1))
                          (ConstantOperand (C.Int 64 2))
                          []
                      ]
                      (Do (Br "next" []))
                  , BasicBlock
                      "next"
                      [ mkName "c" :=
                        Phi i64 [(LocalReference i64 "c", "entry")] []
                      ]
                      (Do (Ret (Just (LocalReference i64 "c")) []))
                  ]
              }
        ]
    }

reuseAcrossFunctions :: AST.Module
reuseAcrossFunctions =
  defaultModule
    { moduleName = "<string>"
    , moduleDefinitions =
        [ GlobalDefinition
            functionDefaults
              { name = "f"
              , returnType = i64
              , basicBlocks =
                  [ BasicBlock
                      "entry"
                      [ "c" :=
                        Add
                          False
                          False
                          (ConstantOperand (C.Int 64 1))
                          (ConstantOperand (C.Int 64 2))
                          []
                      ]
                      (Do (Ret (Just (LocalReference i64 "c")) []))
                  ]
              }
        , GlobalDefinition
            functionDefaults
              { name = "g"
              , returnType = i64
              , basicBlocks =
                  [ BasicBlock
                      "entry"
                      [ "c" :=
                        Add
                          False
                          False
                          (ConstantOperand (C.Int 64 1))
                          (ConstantOperand (C.Int 64 2))
                          []
                      ]
                      (Do (Ret (Just (LocalReference i64 "c")) []))
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

shouldNotThrow :: AST.Module -> IO ()
shouldNotThrow ast = do
  withContext $ \context -> do
    withModuleFromAST context ast (\_ -> return ())

emptyName :: AST.Module
emptyName = defaultModule { moduleDefinitions = defs }
  where defs = execModuleBuilder emptyModuleBuilder (extern "f" [i32, i64] void)

tests :: TestTree
tests =
  testGroup
    "Regression"
    [ testCase
        "no named voids"
        (example1 `shouldThrowEncodeException`
         "Instruction of type void must not have a name: UnName 1 := Store {volatile = False, address = LocalReference (PointerType {pointerAddrSpace = AddrSpace 0}) (UnName 0), value = ConstantOperand (Int {integerBits = 32, integerValue = 42}), maybeAtomicity = Nothing, alignment = 0, metadata = []}")
    , testCase
        "Duplicate definitions are not allowed"
        (duplicateDefinitions `shouldThrowEncodeException`
          "Duplicate definition of local variable: Name \"c\".")
    , testCase
        "Reusing variable names across functions is allowed"
        (shouldNotThrow reuseAcrossFunctions)
    , testCase
        "Empty names do not collide" $ do
           moduleStr <- withContext $ \cxt -> withModuleFromAST cxt emptyName moduleLLVMAssembly
           moduleStr @?= "; ModuleID = '<string>'\nsource_filename = \"<string>\"\n\ndeclare void @f(i32, i64)\n"
--    , testCase
--        "FNeg is parsed correctly" $ do
--           fStr <- B.readFile "test/fneg_test.ll"
--           withContext $ \context -> do
--             a <- withModuleFromLLVMAssembly context fStr moduleAST
--             pure ()
    ]
