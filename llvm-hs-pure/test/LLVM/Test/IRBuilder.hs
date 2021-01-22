{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
module LLVM.Test.IRBuilder
  ( tests
  ) where

import           Test.Tasty
import           Test.Tasty.HUnit

import           LLVM.AST hiding (function)
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.Float as F
import           LLVM.AST.Global (basicBlocks, name, parameters, returnType)
import qualified LLVM.AST.Type as AST
import qualified LLVM.AST.CallingConvention as CC
import qualified LLVM.AST.Instruction as I (function)
import           LLVM.IRBuilder

tests :: TestTree
tests =
  testGroup "IRBuilder" [
    testGroup "module builder"
      [ testCase "builds the simple module" $
        simple @?=
        defaultModule {
          moduleName = "exampleModule",
          moduleDefinitions =
            [ GlobalDefinition functionDefaults {
                name = "add",
                parameters =
                  ( [ Parameter AST.i32 "a_0" []
                    , Parameter AST.i32 "b_0" []
                    ]
                  , False
                  ),
                returnType = AST.i32,
                basicBlocks =
                  [ BasicBlock
                      "entry_0"
                      [ UnName 0 := Add {
                          operand0 = LocalReference AST.i32 "a_0",
                          operand1 = LocalReference AST.i32 "b_0",
                          nsw = False,
                          nuw = False,
                          metadata = []
                        }
                      ]
                      (Do (Ret (Just (LocalReference AST.i32 (UnName 0))) []))
                  ]
              }
            ]
        }
      , testCase "calls constant globals" callWorksWithConstantGlobals
      , testCase "supports recursive function calls" recursiveFunctionCalls
      , testCase "resolves typedefs" resolvesTypeDefs
      , testCase "builds the example" $ do
        let f10 = ConstantOperand (C.Float (F.Double 10))
            fadd a b = FAdd { operand0 = a, operand1 = b, fastMathFlags = noFastMathFlags, metadata = [] }
            add a b = Add { operand0 = a, operand1 = b, nsw = False, nuw = False, metadata = [] }
        example @?=
          defaultModule {
            moduleName = "exampleModule",
            moduleDefinitions =
              [ GlobalDefinition functionDefaults {
                  name = "foo",
                  returnType = AST.double,
                  basicBlocks =
                    [ BasicBlock (UnName 0) [ "xxx_0" := fadd f10 f10]
                        (Do (Ret Nothing []))
                    , BasicBlock
                        "blk_0"
                        [ UnName 1 := fadd f10 f10
                        , UnName 2 := fadd (LocalReference AST.double (UnName 1)) (LocalReference AST.double (UnName 1))
                        , UnName 3 := add (ConstantOperand (C.Int 32 10)) (ConstantOperand (C.Int 32 10))
                        ]
                        (Do (Br "blk_1" []))
                    , BasicBlock
                        "blk_1"
                        [ "c_0" := fadd f10 f10
                        , UnName 4 := fadd (LocalReference AST.double "c_0") (LocalReference AST.double "c_0")
                        ]
                        (Do (Br "blk_2" []))
                    , BasicBlock
                        "blk_2"
                        [ "phi_0" :=
                            Phi
                              AST.double
                              [ ( f10, "blk_0" )
                              , ( f10, "blk_1" )
                              , ( f10, "blk_2" )
                              ]
                              []
                        , UnName 5 := fadd f10 f10
                        , UnName 6 := fadd (LocalReference AST.double (UnName 5)) (LocalReference AST.double (UnName 5))
                        ]
                        (Do (Ret Nothing []))
                    ]
                }
              , GlobalDefinition functionDefaults {
                  name = "bar",
                  returnType = AST.double,
                  basicBlocks =
                    [ BasicBlock
                       (UnName 0)
                       [ UnName 1 := fadd f10 f10
                       , UnName 2 := fadd (LocalReference AST.double (UnName 1)) (LocalReference AST.double (UnName 1))
                       ]
                       (Do (Ret Nothing []))
                    ]
                }
              , GlobalDefinition functionDefaults {
                  name = "baz",
                  parameters =
                    ( [ Parameter AST.i32 (UnName 0) []
                      , Parameter AST.double "arg_0" []
                      , Parameter AST.i32 (UnName 1) []
                      , Parameter AST.double "arg_1" []]
                    , False),
                  returnType = AST.double,
                  basicBlocks =
                    [ BasicBlock
                        (UnName 2)
                        []
                        (Do
                           (Switch
                             (LocalReference AST.i32 (UnName 1))
                             (UnName 3)
                             [ ( C.Int 32 0, UnName 4), ( C.Int 32 1, UnName 7) ] []))
                    , BasicBlock
                        (UnName 3)
                        []
                        (Do (Br (UnName 4) []))
                    , BasicBlock
                        (UnName 4)
                        [ "arg_2" := fadd (LocalReference AST.double "arg_0") f10
                        , UnName 5 := fadd (LocalReference AST.double "arg_2") (LocalReference AST.double "arg_2")
                        , UnName 6 := Select {
                            condition' = ConstantOperand (C.Int 1 0),
                            trueValue = LocalReference AST.double "arg_2",
                            falseValue = LocalReference AST.double (UnName 5),
                            metadata = []
                          }
                        ]
                        (Do (Ret Nothing []))
                    , BasicBlock
                        (UnName 7)
                        [ UnName 8 := GetElementPtr {
                            inBounds = False,
                            address = ConstantOperand (C.Null (AST.ptr (AST.ptr (AST.ptr AST.i32)))),
                            indices =
                              [ ConstantOperand (C.Int 32 10)
                              , ConstantOperand (C.Int 32 20)
                              , ConstantOperand (C.Int 32 30)
                              ],
                            metadata = []
                          }
                        , UnName 9 := GetElementPtr {
                            inBounds = False,
                            address = LocalReference (AST.ptr AST.i32) (UnName 8),
                            indices = [ ConstantOperand (C.Int 32 40) ],
                            metadata = []
                          }
                        ]
                        (Do (Ret Nothing []))
                    ]
                }
              ]
          }
      ]
  ]

recursiveFunctionCalls :: Assertion
recursiveFunctionCalls = do
  m @?= defaultModule
    { moduleName = "exampleModule"
    , moduleDefinitions =
      [ GlobalDefinition functionDefaults
          { returnType = AST.i32
          , name = Name "f"
          , parameters = ([Parameter AST.i32 "a_0" []], False)
          , basicBlocks =
              [ BasicBlock (Name "entry_0")
                 [ UnName 0 := Call
                    { tailCallKind = Nothing
                    , callingConvention = CC.C
                    , returnAttributes = []
                    , I.function =
                        Right (ConstantOperand (C.GlobalReference (AST.ptr (FunctionType AST.i32 [AST.i32] False)) (Name "f")))
                    , arguments = [(LocalReference (IntegerType {typeBits = 32}) (Name "a_0"),[])]
                    , functionAttributes = []
                    , metadata = []
                    }
                 ]
                 (Do (Ret (Just (LocalReference AST.i32 (UnName 0))) []))
              ]
          }
      ]
    }
  where
    m = buildModule "exampleModule" $ mdo
      f <- function "f" [(AST.i32, "a")] AST.i32 $ \[a] -> mdo
        entry <- block `named` "entry"; do
          c <- call f [(a, [])]
          ret c
      pure ()

callWorksWithConstantGlobals :: Assertion
callWorksWithConstantGlobals = do
  funcCall @?= defaultModule
    { moduleName = "exampleModule"
    , moduleDefinitions =
      [ GlobalDefinition functionDefaults {
          returnType = AST.ptr AST.i8,
          name = Name "malloc",
          parameters = ([Parameter (IntegerType {typeBits = 64}) (Name "") []],False),
          basicBlocks = []
        }
      , GlobalDefinition functionDefaults {
          returnType = VoidType,
          name = Name "omg",
          parameters = ([],False),
          basicBlocks = [
            BasicBlock (UnName 0) [
              UnName 1 := Call { tailCallKind = Nothing
                , I.function = Right (
                  ConstantOperand (
                    C.GlobalReference
                      (AST.ptr $ FunctionType {resultType = AST.ptr $ IntegerType {typeBits = 8}, argumentTypes = [IntegerType {typeBits = 64}], isVarArg = False})
                      (Name "malloc")
                    )
                  )
                , callingConvention = CC.C
                , returnAttributes = []
                , arguments = [(ConstantOperand (C.Int {C.integerBits = 64, C.integerValue = 10}),[])]
                , functionAttributes = []
                , metadata = []
                }
              ]
              (Do (Unreachable {metadata' = []}))
          ]
        }
      ]
    }

resolvesTypeDefs :: Assertion
resolvesTypeDefs = do
  buildModule "<string>" builder @?= ast
  where builder = mdo
          pairTy <- typedef "pair" (Just (StructureType False [AST.i32, AST.i32]))
          function "f" [(AST.ptr pairTy, "ptr"), (AST.i32, "x"), (AST.i32, "y")] AST.void $ \[ptr, x, y] -> do
            xPtr <- gep ptr [ConstantOperand (C.Int 32 0), ConstantOperand (C.Int 32 0)]
            yPtr <- gep ptr [ConstantOperand (C.Int 32 0), ConstantOperand (C.Int 32 1)]
            store xPtr 0 x
            store yPtr 0 y
          pure ()
        ast = defaultModule
          { moduleName = "<string>"
          , moduleDefinitions =
            [ TypeDefinition "pair" (Just (StructureType False [AST.i32, AST.i32]))
            , GlobalDefinition functionDefaults
              { name = "f"
              , parameters = ( [ Parameter (AST.ptr (NamedTypeReference "pair")) "ptr_0" []
                               , Parameter AST.i32 "x_0" []
                               , Parameter AST.i32 "y_0" []]
                             , False)
              , returnType = AST.void
              , basicBlocks =
                [ BasicBlock (UnName 0)
                  [ UnName 1 := GetElementPtr
                      { inBounds = False
                      , address = LocalReference (AST.ptr (NamedTypeReference "pair")) "ptr_0"
                      , indices = [ConstantOperand (C.Int 32 0), ConstantOperand (C.Int 32 0)]
                      , metadata = []
                      }
                  , UnName 2 := GetElementPtr
                      { inBounds = False
                      , address = LocalReference (AST.ptr (NamedTypeReference "pair")) "ptr_0"
                      , indices = [ConstantOperand (C.Int 32 0), ConstantOperand (C.Int 32 1)]
                      , metadata = []
                      }
                  , Do $ Store
                      { volatile = False
                      , address = LocalReference (AST.ptr AST.i32) (UnName 1)
                      , value = LocalReference AST.i32 "x_0"
                      , maybeAtomicity = Nothing
                      , alignment = 0
                      , metadata = []
                      }
                  , Do $ Store
                      { volatile = False
                      , address = LocalReference (AST.ptr AST.i32) (UnName 2)
                      , value = LocalReference AST.i32 "y_0"
                      , maybeAtomicity = Nothing
                      , alignment = 0
                      , metadata = []
                      }
                  ]
                  (Do (Ret Nothing []))
                ]
              }
            ]}

simple :: Module
simple = buildModule "exampleModule" $ mdo

  function "add" [(AST.i32, "a"), (AST.i32, "b")] AST.i32 $ \[a, b] -> mdo

    entry <- block `named` "entry"; do
      c <- add a b
      ret c

example :: Module
example = mkModule $ execModuleBuilder emptyModuleBuilder $ mdo

  foo <- function "foo" [] AST.double $ \_ -> mdo
    xxx <- fadd c1 c1 `named` "xxx"

    blk1 <- block `named` "blk"; do
      a <- fadd c1 c1
      b <- fadd a a
      c <- add c2 c2
      br blk2

    blk2 <- block `named` "blk"; do
      a <- fadd c1 c1 `named` "c"
      b <- fadd a a
      br blk3

    blk3 <- block `named` "blk"; do
      l <- phi [(c1, blk1), (c1, blk2), (c1, blk3)] `named` "phi"
      a <- fadd c1 c1
      b <- fadd a a
      retVoid

    pure ()


  function "bar" [] AST.double $ \_ -> mdo

    blk3 <- block; do
      a <- fadd c1 c1
      b <- fadd a a
      retVoid

    pure ()

  function "baz" [(AST.i32, NoParameterName), (AST.double, "arg"), (AST.i32, NoParameterName), (AST.double, "arg")] AST.double $ \[rrr, arg, arg2, arg3] -> mdo

    switch arg2 blk1 [(C.Int 32 0, blk2), (C.Int 32 1, blk3)]

    blk1 <- block; do
      br blk2

    blk2 <- block; do
      a <- fadd arg c1 `named` "arg"
      b <- fadd a a
      select (cons $ C.Int 1 0) a b
      retVoid

    blk3 <- block; do
      let nul = cons $ C.Null $ AST.ptr $ AST.ptr $ AST.ptr $ IntegerType 32
      addr <- gep nul [cons $ C.Int 32 10, cons $ C.Int 32 20, cons $ C.Int 32 30]
      addr' <- gep addr [cons $ C.Int 32 40]
      retVoid

    pure ()
  where
    mkModule ds = defaultModule { moduleName = "exampleModule", moduleDefinitions = ds }
    cons = ConstantOperand

funcCall :: Module
funcCall = mkModule $ execModuleBuilder emptyModuleBuilder $ mdo
  extern "malloc" [AST.i64] (AST.ptr AST.i8)

  let mallocTy = AST.ptr $ AST.FunctionType (AST.ptr AST.i8) [AST.i64] False

  function "omg" [] (AST.void) $ \_ -> do
    let size = int64 10
    call (ConstantOperand $ C.GlobalReference mallocTy "malloc") [(size, [])]
    unreachable
  where
  mkModule ds = defaultModule { moduleName = "exampleModule", moduleDefinitions = ds }

c1 :: Operand
c1 = ConstantOperand $ C.Float (F.Double 10)

c2 :: Operand
c2 = ConstantOperand $ C.Int 32 10
