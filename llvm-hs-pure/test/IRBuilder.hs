{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
module Main
  ( main
  ) where

import           Data.Monoid
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T
import           LLVM.AST hiding (function)
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.Float as F
import           LLVM.AST.Global (basicBlocks, name, parameters, returnType)
import qualified LLVM.AST.Type as AST
import           Test.Hspec hiding (example)

import           LLVM.IRBuilder

main :: IO ()
main =
  hspec $ do
    describe "module builder" $ do
      it "builds the simple module" $
        simple `shouldBe`
        defaultModule {
          moduleName = "exampleModule",
          moduleDefinitions =
            [ GlobalDefinition functionDefaults {
                name = "add",
                parameters =
                  ( [ Parameter AST.i32 "a" []
                    , Parameter AST.i32 "b" []
                    ]
                  , False
                  ),
                returnType = AST.i32,
                basicBlocks =
                  [ BasicBlock
                      "entry"
                      [ UnName 0 := Add {
                          operand0 = LocalReference AST.i32 "a",
                          operand1 = LocalReference AST.i32 "b",
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
      it "builds the example" $ do
        let f10 = ConstantOperand (C.Float (F.Double 10))
            fadd a b = FAdd { operand0 = a, operand1 = b, fastMathFlags = noFastMathFlags, metadata = [] }
            add a b = Add { operand0 = a, operand1 = b, nsw = False, nuw = False, metadata = [] }
        example `shouldBe`
          defaultModule {
            moduleName = "exampleModule",
            moduleDefinitions =
              [ GlobalDefinition functionDefaults {
                  name = "foo",
                  returnType = AST.double,
                  basicBlocks =
                    [ BasicBlock (UnName 0) [ "xxx" := fadd f10 f10]
                        (Do (Ret Nothing []))
                    , BasicBlock
                        "blk"
                        [ UnName 1 := fadd f10 f10
                        , UnName 2 := fadd (LocalReference AST.double (UnName 1)) (LocalReference AST.double (UnName 1))
                        , UnName 3 := add (ConstantOperand (C.Int 32 10)) (ConstantOperand (C.Int 32 10))
                        ]
                        (Do (Br "blk1" []))
                    , BasicBlock
                        "blk1"
                        [ "c" := fadd f10 f10
                        , UnName 4 := fadd (LocalReference AST.double "c") (LocalReference AST.double "c")
                        ]
                        (Do (Br "blk2" []))
                    , BasicBlock
                        "blk2"
                        [ "phi" :=
                            Phi
                              AST.double
                              [ ( f10, "blk" )
                              , ( f10, "blk1" )
                              , ( f10, "blk2" )
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
                      , Parameter AST.double "arg" []
                      , Parameter AST.i32 (UnName 1) []
                      , Parameter AST.double "arg1" []]
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
                        [ "arg2" := fadd (LocalReference AST.double "arg") f10
                        , UnName 5 := fadd (LocalReference AST.double "arg2") (LocalReference AST.double "arg2")
                        , UnName 6 := Select {
                            condition' = ConstantOperand (C.Int 1 0),
                            trueValue = LocalReference AST.double "arg2",
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

c1 :: Operand
c1 = ConstantOperand $ C.Float (F.Double 10)

c2 :: Operand
c2 = ConstantOperand $ C.Int 32 10
