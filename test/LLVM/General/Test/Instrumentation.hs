module LLVM.General.Test.Instrumentation where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit

import LLVM.General.Test.Support

import Data.Functor
import qualified Data.Set as Set
import qualified Data.Map as Map

import LLVM.General.Module
import LLVM.General.Context
import LLVM.General.PassManager
import LLVM.General.Transforms

import LLVM.General.AST as A
import LLVM.General.AST.Type
import LLVM.General.AST.Name
import LLVM.General.AST.AddrSpace
import LLVM.General.AST.DataLayout
import qualified LLVM.General.AST.IntegerPredicate as IPred
import qualified LLVM.General.AST.Linkage as L
import qualified LLVM.General.AST.Visibility as V
import qualified LLVM.General.AST.CallingConvention as CC
import qualified LLVM.General.AST.Attribute as A
import qualified LLVM.General.AST.Global as G
import qualified LLVM.General.AST.Constant as C

instrument :: PassManagerSpecification s => s -> A.Module -> IO A.Module
instrument s m = withContext $ \context -> withModuleFromAST' context m $ \mIn' -> do
  withPassManager s $ \pm -> runPassManager pm mIn'
  moduleAST mIn'

tests = testGroup "Instrumentation" [
  testCase "EdgeProfiler" $ do
    let ast = 
         Module "<string>" Nothing Nothing [
          GlobalDefinition $ Function L.External V.Default CC.C [] (IntegerType 32) (Name "foo") ([
              Parameter (IntegerType 128) (Name "x") []
             ],False)
           [] 
           Nothing 0         
           [
            BasicBlock (UnName 0) [] (Do $ Br (Name "checkDone") []),
            BasicBlock (Name "checkDone") [
              UnName 1 := Phi {
               type' = IntegerType 128,
               incomingValues = [
                (LocalReference (Name "x"), UnName 0),
                (LocalReference (Name "x'"), Name "even"),
                (LocalReference (Name "x''"), Name "odd")
               ],
               metadata = []
              },
              Name "count" := Phi {
               type' = IntegerType 32,
               incomingValues = [
                (ConstantOperand (C.Int 32 1), UnName 0),
                (LocalReference (Name "count'"), Name "even"),
                (LocalReference (Name "count'"), Name "odd")
               ],
               metadata = []
              },
              Name "count'" := Add False False (LocalReference (Name "count")) (ConstantOperand (C.Int 32 1)) [],
              Name "is one" := ICmp IPred.EQ (LocalReference (UnName 1)) (ConstantOperand (C.Int 128 1)) []
            ] (
              Do $ CondBr (LocalReference (Name "is one")) (Name "done") (Name "checkOdd") []
            ),
            BasicBlock (Name "checkOdd") [
              Name "is odd" := Trunc (LocalReference (UnName 1)) (IntegerType 1) []
            ] (
             Do $ CondBr (LocalReference (Name "is odd")) (Name "odd") (Name "even") []
            ),
            BasicBlock (Name "even") [
              Name "x'" := UDiv True (LocalReference (UnName 1)) (ConstantOperand (C.Int 128 2)) []
            ] (
              Do $ Br (Name "checkDone") []
            ),
            BasicBlock (Name "odd") [
              UnName 2 := Mul False False (LocalReference (UnName 1)) (ConstantOperand (C.Int 128 3)) [],
              Name "x''" := Add False False (LocalReference (UnName 2)) (ConstantOperand (C.Int 128 1)) []
            ] (
              Do $ Br (Name "checkDone") []
            ),
            BasicBlock (Name "done") [
            ] (
              Do $ Ret (Just (LocalReference (Name "count'"))) []
            )
           ],
          GlobalDefinition $ Function L.External V.Default CC.C [] (IntegerType 32) (Name "main") ([
              Parameter (IntegerType 32) (Name "argc") [],
              Parameter (PointerType (PointerType (IntegerType 8) (AddrSpace 0)) (AddrSpace 0)) (Name "argv") []
             ],False)
           [] 
           Nothing 0         
           [
            BasicBlock (UnName 0) [
              UnName 1 := Call {
                isTailCall = False,
                callingConvention = CC.C,
                returnAttributes = [],
                function = Right (ConstantOperand (C.GlobalReference (Name "foo"))),
                arguments = [
                 (ConstantOperand (C.Int 128 9491828328), [])
                ],
                functionAttributes = [],
                metadata = []
              }
            ] (
              Do $ Ret (Just (LocalReference (UnName 1))) []
            )
           ]
          ]

    mOut <- instrument [EdgeProfiler] ast

    let counterBump a b c = [
          Name ("OldFuncCounter" ++ a)  := Load {
            volatile = False,
            address = ConstantOperand C.GetElementPtr {
              C.inBounds = True,
              C.address = C.GlobalReference (Name "EdgeProfCounters"),
              C.indices = [ (C.Int 32 0), (C.Int 32 b) ]
            },
            maybeAtomicity = Nothing,
            alignment = 0,
            metadata = []
          },
          Name ("NewFuncCounter" ++ c) := Add {
            nsw = False,
            nuw = False,
            operand0 = LocalReference (Name ("OldFuncCounter" ++ a)),
            operand1 = ConstantOperand (C.Int 32 1),
            metadata = []
          },
          Do Store {
            volatile = False,
            address = ConstantOperand C.GetElementPtr {
              C.inBounds = True,
              C.address = C.GlobalReference (Name "EdgeProfCounters"),
              C.indices = [ (C.Int 32 0), (C.Int 32 b) ]
            },
            value = LocalReference (Name ("NewFuncCounter" ++ c)),
            maybeAtomicity = Nothing,
            alignment = 0,
            metadata = []
          }
         ]

    mOut `assertEqPretty` Module "<string>" Nothing Nothing [
        GlobalDefinition G.globalVariableDefaults {
          G.name = Name "EdgeProfCounters",
          G.linkage = L.Internal,
          G.type' = ArrayType 9 (IntegerType 32),
          G.initializer = Just $ C.Null (ArrayType 9 (IntegerType 32))
        },
        GlobalDefinition G.functionDefaults {
          G.returnType = IntegerType 32,
          G.name = Name "foo",
          G.parameters = ([G.Parameter (IntegerType 128) (Name "x") []], False),
          G.basicBlocks = [
            G.BasicBlock (UnName 0) (
              counterBump "" 0 "" ++ counterBump "1" 1 "2"
            ) (Do Br { dest = Name "checkDone", metadata' = [] }),
            G.BasicBlock (Name "checkDone") [
              UnName 1 := Phi {
                type' = IntegerType 128,
                incomingValues = [
                  (LocalReference (Name "x"), UnName 0),
                  (LocalReference (Name "x'"), Name "even"),
                  (LocalReference (Name "x''"), Name "odd")
                ],
                metadata = []
              },
              Name "count" := Phi {
                type' = IntegerType 32,
                incomingValues = [
                  (ConstantOperand (C.Int 32 1), UnName 0),
                  (LocalReference (Name "count'"), Name "even"),
                  (LocalReference (Name "count'"), Name "odd")
                ],
                metadata = []
              },
              Name "count'" := Add False False (LocalReference (Name "count")) (ConstantOperand (C.Int 32 1)) [],
              Name "is one" := ICmp IPred.EQ (LocalReference (UnName 1)) (ConstantOperand (C.Int 128 1)) []
            ] (
              Do $ CondBr (LocalReference (Name "is one")) (Name "done") (Name "checkOdd") []
            ),
            G.BasicBlock (Name "checkOdd") (
            counterBump "5" 3 "6" ++ [ 
               Name "is odd" := Trunc (LocalReference (UnName 1)) (IntegerType 1) []
            ]) (
              Do CondBr {
                condition = LocalReference (Name "is odd"),
                trueDest = Name "odd",
                falseDest = Name "even",
                metadata' = []
              }
            ),
            G.BasicBlock (Name "even") (
            counterBump "9" 5 "10" ++ [
              Name "x'" := UDiv True (LocalReference (UnName 1)) (ConstantOperand (C.Int 128 2)) []
            ] ++ counterBump "11" 6 "12" 
            ) (Do Br { dest = Name "checkDone", metadata' = [] }),
            G.BasicBlock (Name "odd") (
            counterBump "7" 4 "8" ++ [
              UnName 2 := Mul False False (LocalReference (UnName 1)) (ConstantOperand (C.Int 128 3)) [],
              Name "x''" := Add False False (LocalReference (UnName 2)) (ConstantOperand (C.Int 128 1)) []
            ] ++ counterBump "13" 7 "14"
            ) (Do Br { dest = Name "checkDone", metadata' = [] }),
            G.BasicBlock (Name "done") (
            counterBump "3" 2 "4"
            ) (
              Do $ Ret (Just (LocalReference (Name "count'"))) []
            )
          ]
        },
        GlobalDefinition G.functionDefaults {
          G.returnType = (IntegerType 32),
          G.name = Name "main",
          G.parameters = (
            [
              G.Parameter (IntegerType 32) (Name "argc") [],
              G.Parameter (PointerType (PointerType (IntegerType 8) (AddrSpace 0)) (AddrSpace 0)) (Name "argv") []
            ],
            False
          ),
          G.basicBlocks = [
            G.BasicBlock (UnName 0) ([
              Name "newargc" := Call {
                isTailCall = False,
                callingConvention = CC.C,
                returnAttributes = [],
                function = Right (ConstantOperand (C.GlobalReference (Name "llvm_start_edge_profiling"))),
                arguments = [
                  (LocalReference (Name "argc"), []),
                  (LocalReference (Name "argv"), []),
                  (
                    ConstantOperand C.GetElementPtr {
                      C.inBounds = True,
                      C.address = C.GlobalReference (Name "EdgeProfCounters"),
                      C.indices = [ (C.Int 32 0), (C.Int 32 0) ]
                    },
                    []
                  ),
                  ((ConstantOperand (C.Int 32 9)), [])
                ],
                functionAttributes = [],
                metadata = []
              }
            ] ++ counterBump "" 8 "" ++ [
              UnName 1 := Call {
                isTailCall = False,
                callingConvention = CC.C,
                returnAttributes = [],
                function = Right (ConstantOperand (C.GlobalReference (Name "foo"))),
                arguments = [ (ConstantOperand (C.Int 128 9491828328), []) ],
                functionAttributes = [],
                metadata = []
              }
            ]) (Do $ Ret (Just (LocalReference (UnName 1))) [])
          ]
        },
        GlobalDefinition G.functionDefaults {
          G.returnType = IntegerType 32,
          G.name = Name "llvm_start_edge_profiling",
          G.parameters = (
            [
              G.Parameter (IntegerType 32) (UnName 0) [],
              G.Parameter (PointerType (PointerType (IntegerType 8) (AddrSpace 0)) (AddrSpace 0)) (UnName 1) [],
              G.Parameter (PointerType (IntegerType 32) (AddrSpace 0)) (UnName 2) [],
              G.Parameter (IntegerType 32) (UnName 3) []
            ],
            False
          )
        }
      ]
 ]
