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
import qualified LLVM.AST.Global
import           LLVM.AST.Linkage (Linkage(..))
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
                LLVM.AST.Global.name = "add",
                LLVM.AST.Global.parameters =
                  ( [ Parameter AST.i32 "a_0" []
                    , Parameter AST.i32 "b_0" []
                    ]
                  , False
                  ),
                LLVM.AST.Global.returnType = AST.i32,
                LLVM.AST.Global.basicBlocks =
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
      , testCase "resolves constant typedefs" resolvesConstantTypeDefs
      , testCase "handling of terminator" terminatorHandling
      , testCase "builds the example" $ do
        let f10 = ConstantOperand (C.Float (F.Double 10))
            fadd a b = FAdd { operand0 = a, operand1 = b, fastMathFlags = noFastMathFlags, metadata = [] }
            add a b = Add { operand0 = a, operand1 = b, nsw = False, nuw = False, metadata = [] }
        example @?=
          defaultModule {
            moduleName = "exampleModule",
            moduleDefinitions =
              [ GlobalDefinition functionDefaults {
                  LLVM.AST.Global.name = "foo",
                  LLVM.AST.Global.returnType = AST.double,
                  LLVM.AST.Global.basicBlocks =
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
                  LLVM.AST.Global.name = "bar",
                  LLVM.AST.Global.returnType = AST.double,
                  LLVM.AST.Global.basicBlocks =
                    [ BasicBlock
                       (UnName 0)
                       [ UnName 1 := fadd f10 f10
                       , UnName 2 := fadd (LocalReference AST.double (UnName 1)) (LocalReference AST.double (UnName 1))
                       ]
                       (Do (Ret Nothing []))
                    ]
                }
              , GlobalDefinition functionDefaults {
                  LLVM.AST.Global.name = "baz",
                  LLVM.AST.Global.parameters =
                    ( [ Parameter AST.i32 (UnName 0) []
                      , Parameter AST.double "arg_0" []
                      , Parameter AST.i32 (UnName 1) []
                      , Parameter AST.double "arg_1" []]
                    , False),
                  LLVM.AST.Global.returnType = AST.double,
                  LLVM.AST.Global.basicBlocks =
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
          { LLVM.AST.Global.returnType = AST.i32
          , LLVM.AST.Global.name = Name "f"
          , LLVM.AST.Global.parameters = ([Parameter AST.i32 "a_0" []], False)
          , LLVM.AST.Global.basicBlocks =
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
          LLVM.AST.Global.returnType = AST.ptr AST.i8,
          LLVM.AST.Global.name = Name "malloc",
          LLVM.AST.Global.parameters = ([Parameter (IntegerType {typeBits = 64}) (Name "") []],False),
          LLVM.AST.Global.basicBlocks = []
        }
      , GlobalDefinition functionDefaults {
          LLVM.AST.Global.returnType = VoidType,
          LLVM.AST.Global.name = Name "omg",
          LLVM.AST.Global.parameters = ([],False),
          LLVM.AST.Global.basicBlocks = [
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
          function "g" [(pairTy, "pair")] AST.i32 $ \[pair] -> do
            x <- extractValue pair [0]
            y <- extractValue pair [1]
            z <- add x y
            ret z
          pure ()
        ast = defaultModule
          { moduleName = "<string>"
          , moduleDefinitions =
            [ TypeDefinition "pair" (Just (StructureType False [AST.i32, AST.i32]))
            , GlobalDefinition functionDefaults
              { LLVM.AST.Global.name = "f"
              , LLVM.AST.Global.parameters = ( [ Parameter (AST.ptr (NamedTypeReference "pair")) "ptr_0" []
                               , Parameter AST.i32 "x_0" []
                               , Parameter AST.i32 "y_0" []]
                             , False)
              , LLVM.AST.Global.returnType = AST.void
              , LLVM.AST.Global.basicBlocks =
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
            , GlobalDefinition functionDefaults
              { LLVM.AST.Global.name = "g"
              , LLVM.AST.Global.parameters = ( [Parameter (NamedTypeReference "pair") "pair_0" []]
                             , False)
              , LLVM.AST.Global.returnType = AST.i32
              , LLVM.AST.Global.basicBlocks =
                [ BasicBlock (UnName 0)
                  [ UnName 1 := ExtractValue
                      { aggregate = LocalReference (NamedTypeReference "pair") "pair_0"
                      , indices' = [0]
                      , metadata = []
                      }
                  , UnName 2 := ExtractValue
                      { aggregate = LocalReference (NamedTypeReference "pair") "pair_0"
                      , indices' = [1]
                      , metadata = []
                      }
                  , UnName 3 := Add
                      { nsw = False
                      , nuw = False
                      , operand0 = LocalReference AST.i32 (UnName 1)
                      , operand1 = LocalReference AST.i32 (UnName 2)
                      , metadata = []
                      }
                  ]
                  (Do (Ret (Just (LocalReference AST.i32 (UnName 3))) []))
                ]
              }
            ]}

resolvesConstantTypeDefs :: Assertion
resolvesConstantTypeDefs = do
  buildModule "<string>" builder @?= ast
  where builder = mdo
          pairTy <- typedef "pair" (Just (StructureType False [AST.i32, AST.i32]))
          globalPair <- global "gpair" pairTy (C.AggregateZero pairTy)
          function "f" [(AST.i32, "x"), (AST.i32, "y")] AST.void $ \[x, y] -> do
            let ptr = ConstantOperand $ C.GlobalReference (AST.ptr pairTy) "gpair"
            xPtr <- gep ptr [ConstantOperand (C.Int 32 0), ConstantOperand (C.Int 32 0)]
            yPtr <- gep ptr [ConstantOperand (C.Int 32 0), ConstantOperand (C.Int 32 1)]
            store xPtr 0 x
            store yPtr 0 y
          function "g" [] AST.i32 $ \[] -> do
            pair <- load (ConstantOperand $ C.GlobalReference (AST.ptr pairTy) "gpair") 0
            x <- extractValue pair [0]
            y <- extractValue pair [1]
            z <- add x y
            ret z
          pure ()
        ast = defaultModule
          { moduleName = "<string>"
          , moduleDefinitions =
            [ TypeDefinition "pair" (Just (StructureType False [AST.i32, AST.i32]))
            , GlobalDefinition globalVariableDefaults
              { LLVM.AST.Global.name = "gpair"
              , LLVM.AST.Global.type' = NamedTypeReference "pair"
              , LLVM.AST.Global.linkage = External
              , LLVM.AST.Global.initializer = Just (C.AggregateZero (NamedTypeReference "pair"))
              }
            , GlobalDefinition functionDefaults
              { LLVM.AST.Global.name = "f"
              , LLVM.AST.Global.parameters = ( [ Parameter AST.i32 "x_0" []
                                               , Parameter AST.i32 "y_0" []]
                             , False)
              , LLVM.AST.Global.returnType = AST.void
              , LLVM.AST.Global.basicBlocks =
                [ BasicBlock (UnName 0)
                  [ UnName 1 := GetElementPtr
                      { inBounds = False
                      , address = ConstantOperand (C.GlobalReference (AST.ptr (NamedTypeReference "pair")) "gpair")
                      , indices = [ConstantOperand (C.Int 32 0), ConstantOperand (C.Int 32 0)]
                      , metadata = []
                      }
                  , UnName 2 := GetElementPtr
                      { inBounds = False
                      , address = ConstantOperand (C.GlobalReference (AST.ptr (NamedTypeReference "pair")) "gpair")
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
            , GlobalDefinition functionDefaults
              { LLVM.AST.Global.name = "g"
              , LLVM.AST.Global.parameters = ([], False)
              , LLVM.AST.Global.returnType = AST.i32
              , LLVM.AST.Global.basicBlocks =
                [ BasicBlock (UnName 0)
                  [ UnName 1 := Load
                      { volatile = False,
                        address = ConstantOperand (C.GlobalReference (AST.ptr (NamedTypeReference "pair")) "gpair"),
                        maybeAtomicity = Nothing,
                        alignment = 0,
                        metadata = []
                      }
                  , UnName 2 := ExtractValue
                      { aggregate = LocalReference (NamedTypeReference "pair") (UnName 1)
                      , indices' = [0]
                      , metadata = []
                      }
                  , UnName 3 := ExtractValue
                      { aggregate = LocalReference (NamedTypeReference "pair") (UnName 1)
                      , indices' = [1]
                      , metadata = []
                      }
                  , UnName 4 := Add
                      { nsw = False
                      , nuw = False
                      , operand0 = LocalReference AST.i32 (UnName 2)
                      , operand1 = LocalReference AST.i32 (UnName 3)
                      , metadata = []
                      }
                  ]
                  (Do (Ret (Just (LocalReference AST.i32 (UnName 4))) []))
                ]
              }
            ]}

terminatorHandling :: Assertion
terminatorHandling = do
  firstTerminatorWins @?= firstWinsAst
  terminatorsCompose @?= terminatorsComposeAst
  nestedControlFlowWorks @?= nestedControlFlowAst
  where
    firstTerminatorWins = buildModule "firstTerminatorWinsModule" $ mdo
      function "f" [(AST.i32, "a"), (AST.i32, "b")] AST.i32 $ \[a, b] -> mdo

        entry <- block `named` "entry"; do
          c <- add a b
          d <- add a c
          ret c
          ret d
    terminatorsCompose = buildModule "terminatorsComposeModule" $ mdo
      function "f" [(AST.i1, "a")] AST.i1 $ \[a] -> mdo

        entry <- block `named` "entry"; do
          if' a $ do
            ret (bit 0)

          ret (bit 1)
    nestedControlFlowWorks = buildModule "nestedControlFlowWorksModule" $ mdo
      function "f" [(AST.i1, "a"), (AST.i1, "b")] AST.i1 $ \[a, b] -> mdo

        entry <- block `named` "entry"; do
          if' a $ do
            if' b $ do
              ret (bit 0)

          ret (bit 1)
    if' cond asm = mdo
      condBr cond ifBlock end
      ifBlock <- block `named` "if.begin"
      asm
      br end
      end <- block `named` "if.end"
      return ()

    firstWinsAst = defaultModule
      { moduleName = "firstTerminatorWinsModule"
      , moduleDefinitions =
        [ GlobalDefinition functionDefaults
          { LLVM.AST.Global.name = "f"
          , LLVM.AST.Global.parameters = ([ Parameter AST.i32 "a_0" [], Parameter AST.i32 "b_0" []], False)
          , LLVM.AST.Global.returnType = AST.i32
          , LLVM.AST.Global.basicBlocks =
            [ BasicBlock (Name "entry_0")
              [ UnName 0 := Add { nsw = False, nuw = False, metadata = []
                                , operand0 = LocalReference (IntegerType {typeBits = 32}) (Name "a_0")
                                , operand1 = LocalReference (IntegerType {typeBits = 32}) (Name "b_0")
                                }
              , UnName 1 := Add { nsw = False, nuw = False, metadata = []
                                , operand0 = LocalReference (IntegerType {typeBits = 32}) (Name "a_0")
                                , operand1 = LocalReference (IntegerType {typeBits = 32}) (UnName 0)
                                }
              ]
              (Do (Ret {returnOperand = Just (LocalReference (IntegerType {typeBits = 32}) (UnName 0)), metadata' = []}))]
          }
        ]}
    terminatorsComposeAst = defaultModule
      { moduleName = "terminatorsComposeModule"
      , moduleDefinitions =
        [ GlobalDefinition functionDefaults
          { LLVM.AST.Global.name = "f"
          , LLVM.AST.Global.parameters = ([ Parameter AST.i1 "a_0" []], False)
          , LLVM.AST.Global.returnType = AST.i1
          , LLVM.AST.Global.basicBlocks =
            [ BasicBlock (Name "entry_0")
              []
              (Do (CondBr {condition = LocalReference (IntegerType {typeBits = 1}) (Name "a_0")
                          , trueDest = Name "if.begin_0"
                          , falseDest = Name "if.end_0", metadata' = []}))
                          , BasicBlock (Name "if.begin_0")
                            []
                            (Do (Ret {returnOperand = Just (ConstantOperand (C.Int {C.integerBits = 1, C.integerValue = 0})), metadata' = []}))
                          , BasicBlock (Name "if.end_0")
                            []
                            (Do (Ret {returnOperand = Just (ConstantOperand (C.Int {C.integerBits = 1, C.integerValue = 1})), metadata' = []}))]
          }
        ]}
    nestedControlFlowAst = defaultModule
      { moduleName = "nestedControlFlowWorksModule"
      , moduleDefinitions =
        [ GlobalDefinition functionDefaults
          { LLVM.AST.Global.name = "f"
          , LLVM.AST.Global.parameters = ([ Parameter AST.i1 "a_0" [], Parameter AST.i1 "b_0" []], False)
          , LLVM.AST.Global.returnType = AST.i1
          , LLVM.AST.Global.basicBlocks =
            [ BasicBlock (Name "entry_0")
              []
              (Do (CondBr { condition = LocalReference (IntegerType {typeBits = 1}) (Name "a_0")
                          , trueDest = Name "if.begin_0"
                          , falseDest = Name "if.end_1"
                          , metadata' = []}))
                          , BasicBlock (Name "if.begin_0") [] (Do (CondBr { condition = LocalReference (IntegerType {typeBits = 1}) (Name "b_0")
                                                                          , trueDest = Name "if.begin_1"
                                                                          , falseDest = Name "if.end_0"
                                                                          , metadata' = []}))
                                                                          , BasicBlock (Name "if.begin_1") [] (Do (Ret {returnOperand = Just (ConstantOperand (C.Int {C.integerBits = 1, C.integerValue = 0})), metadata' = []}))
                                                                          , BasicBlock (Name "if.end_0") [] (Do (Br {dest = Name "if.end_1", metadata' = []}))
                          , BasicBlock (Name "if.end_1") [] (Do (Ret {returnOperand = Just (ConstantOperand (C.Int {C.integerBits = 1, C.integerValue = 1})), metadata' = []}))
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
