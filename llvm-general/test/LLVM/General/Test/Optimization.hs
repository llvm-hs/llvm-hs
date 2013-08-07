module LLVM.General.Test.Optimization where

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
import LLVM.General.Target

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

import qualified LLVM.General.Relocation as R
import qualified LLVM.General.CodeModel as CM
import qualified LLVM.General.CodeGenOpt as CGO

handAST = 
  Module "<string>" Nothing Nothing [
      GlobalDefinition $ Function L.External V.Default CC.C [] (IntegerType 32) (Name "foo") ([
          Parameter (IntegerType 32) (Name "x") []
         ],False)
        [A.NoUnwind, A.ReadNone, A.UWTable] 
        Nothing 0
        [
        BasicBlock (UnName 0) [
         UnName 1 := Mul {
           nsw = False,
           nuw = False,
           operand0 = ConstantOperand (C.Int 32 6),
           operand1 = ConstantOperand (C.Int 32 7),
           metadata = []
         }
         ] (
           Do $ Br (Name "here") []
         ),
        BasicBlock (Name "here") [
         Name "go" := ICmp {
           iPredicate = IPred.EQ,
           operand0 = LocalReference (UnName 1),
           operand1 = ConstantOperand (C.Int 32 42),
           metadata = []
         }
         ] (
            Do $ CondBr {
              condition = LocalReference (Name "go"),
              trueDest = Name "take",
              falseDest = Name "done",
              metadata' = []
            }
         ),
        BasicBlock (Name "take") [
         UnName 2 := Sub {
           nsw = False,
           nuw = False,
           operand0 = LocalReference (Name "x"),
           operand1 = LocalReference (Name "x"),
           metadata = []
         }
         ] (
           Do $ Br (Name "done") []
         ),
        BasicBlock (Name "done") [
         Name "r" := Phi {
           type' = IntegerType 32,
           incomingValues = [
             (LocalReference (UnName 2), Name "take"),
             (ConstantOperand (C.Int 32 57), Name "here")
           ],
           metadata = []
         }
         ] (
           Do $ Ret (Just (LocalReference (Name "r"))) []
         )
       ]
      ]

optimize :: PassSetSpec -> A.Module -> IO A.Module
optimize pss m = withContext $ \context -> withModuleFromAST' context m $ \mIn' -> do
  withPassManager pss $ \pm -> runPassManager pm mIn'
  moduleAST mIn'

tests = testGroup "Optimization" [
  testCase "curated" $ do
    mOut <- optimize defaultCuratedPassSetSpec handAST

    mOut @?= Module "<string>" Nothing Nothing [
      GlobalDefinition $ Function L.External V.Default CC.C [] (IntegerType 32) (Name "foo") ([
          Parameter (IntegerType 32) (Name "x") []
         ],False)
       [A.NoUnwind, A.ReadNone, A.UWTable] 
       Nothing 0         
       [
        BasicBlock (Name "here") [
           ] (
           Do $ Ret (Just (ConstantOperand (C.Int 32 0))) []
         )
       ]
      ],

  testGroup "individual" [
    testCase "ConstantPropagation" $ do
      mOut <- optimize defaultPassSetSpec { transforms = [ConstantPropagation] } handAST

      mOut @?= Module "<string>" Nothing Nothing [
        GlobalDefinition $ Function L.External V.Default CC.C [] (IntegerType 32) (Name "foo") ([
            Parameter (IntegerType 32) (Name "x") []
           ],False)
         [A.NoUnwind, A.ReadNone, A.UWTable] 
         Nothing 0         
         [
          BasicBlock (UnName 0) [] (Do $ Br (Name "here") []),
          BasicBlock (Name "here") [] (
             Do $ CondBr {
               condition = ConstantOperand (C.Int 1 1),
               trueDest = Name "take", 
               falseDest = Name "done",
               metadata' = []
             }
          ),
          BasicBlock (Name "take") [
           UnName 1 := Sub {
             nsw = False,
             nuw = False,
             operand0 = LocalReference (Name "x"),
             operand1 = LocalReference (Name "x"),
             metadata = []
            }
          ] (
           Do $ Br (Name "done") []
          ),
          BasicBlock (Name "done") [
           Name "r" := Phi {
             type' = IntegerType 32,
             incomingValues = [(LocalReference (UnName 1),Name "take"),(ConstantOperand (C.Int 32 57), Name "here")],
             metadata = []
            }
          ] (
            Do $ Ret (Just (LocalReference (Name "r"))) []
          )
         ]
        ],

    testCase "BasicBlockVectorization" $ do
      let
        mIn = Module "<string>" Nothing Nothing [
         GlobalDefinition $ Function L.External V.Default CC.C [] (FloatingPointType 64 IEEE) (Name "foo") ([
             Parameter (FloatingPointType 64 IEEE) (Name "a1") [],
             Parameter (FloatingPointType 64 IEEE) (Name "a2") [],
             Parameter (FloatingPointType 64 IEEE) (Name "b1") [],
             Parameter (FloatingPointType 64 IEEE) (Name "b2") []
            ],False)
          [] 
          Nothing 0         
          [
           BasicBlock (UnName 0) [
             Name "x1" := FSub { 
                        operand0 = LocalReference (Name "a1"), 
                        operand1 = LocalReference (Name "b1"),
                        metadata = []
                      },
             Name "x2" := FSub { 
                        operand0 = LocalReference (Name "a2"), 
                        operand1 = LocalReference (Name "b2"),
                        metadata = []
                      },
             Name "y1" := FMul { 
                        operand0 = LocalReference (Name "x1"), 
                        operand1 = LocalReference (Name "a1"),
                        metadata = []
                      },
             Name "y2" := FMul { 
                        operand0 = LocalReference (Name "x2"), 
                        operand1 = LocalReference (Name "a2"),
                        metadata = []
                      },
             Name "z1" := FAdd { 
                        operand0 = LocalReference (Name "y1"), 
                        operand1 = LocalReference (Name "b1"),
                        metadata = []
                      },
             Name "z2" := FAdd { 
                        operand0 = LocalReference (Name "y2"), 
                        operand1 = LocalReference (Name "b2"),
                        metadata = []
                      },
             Name "r" := FMul {
                        operand0 = LocalReference (Name "z1"), 
                        operand1 = LocalReference (Name "z2"),
                        metadata = []
                      }
           ] (Do $ Ret (Just (LocalReference (Name "r"))) [])
          ]
         ]
      mOut <- 
        optimize (defaultPassSetSpec { transforms = [ defaultVectorizeBasicBlocks { requiredChainDepth = 3 }, InstructionCombining, GlobalValueNumbering False ] }) mIn
      mOut @?= Module "<string>" Nothing Nothing [
       GlobalDefinition $ Function 
        L.External V.Default CC.C [] (FloatingPointType 64 IEEE) (Name "foo") ([
              Parameter (FloatingPointType 64 IEEE) (Name "a1") [],
              Parameter (FloatingPointType 64 IEEE) (Name "a2") [],
              Parameter (FloatingPointType 64 IEEE) (Name "b1") [],
              Parameter (FloatingPointType 64 IEEE) (Name "b2") []
             ],False)
             []
             Nothing 0
          [
           BasicBlock (UnName 0) [
             Name "x1.v.i1.1" := InsertElement {
               vector = ConstantOperand (C.Undef (VectorType 2 (FloatingPointType 64 IEEE))),
               element = LocalReference (Name "b1"),
               index = ConstantOperand (C.Int 32 0),
               metadata = []
              },
             Name "x1.v.i1.2" := InsertElement {
               vector = LocalReference (Name "x1.v.i1.1"),
               element = LocalReference (Name "b2"),
               index = ConstantOperand (C.Int 32 1),
               metadata = []
              },
             Name "x1.v.i0.1" := InsertElement {
               vector = ConstantOperand (C.Undef (VectorType 2 (FloatingPointType 64 IEEE))),
               element = LocalReference (Name "a1"),
               index = ConstantOperand (C.Int 32 0),
               metadata = []
              },
             Name "x1.v.i0.2" := InsertElement {
               vector = LocalReference (Name "x1.v.i0.1"),
               element = LocalReference (Name "a2"),
               index = ConstantOperand (C.Int 32 1),
               metadata = []
              },
             Name "x1" := FSub {
               operand0 = LocalReference (Name "x1.v.i0.2"),
               operand1 = LocalReference (Name "x1.v.i1.2"),
               metadata = []
              },
             Name "y1" := FMul {
               operand0 = LocalReference (Name "x1"),
               operand1 = LocalReference (Name "x1.v.i0.2"),
               metadata = []
              },
             Name "z1" := FAdd {
               operand0 = LocalReference (Name "y1"),
               operand1 = LocalReference (Name "x1.v.i1.2"),
               metadata = []
              },
             Name "z1.v.r1" := ExtractElement {
               vector = LocalReference (Name "z1"),
               index = ConstantOperand (C.Int 32 0),
               metadata = []
              },
             Name "z1.v.r2" := ExtractElement {
               vector = LocalReference (Name "z1"),
               index = ConstantOperand (C.Int 32 1),
               metadata = []
              },
             Name "r" := FMul {
               operand0 = LocalReference (Name "z1.v.r1"),
               operand1 = LocalReference (Name "z1.v.r2"),
               metadata = []
              }
            ] (
             Do $ Ret (Just (LocalReference (Name "r"))) []
            )
          ]
        ],
      
    testCase "LowerInvoke" $ do
      -- This test doesn't test much about what LowerInvoke does, just that it seems to work.
      -- The pass seems to be quite deeply dependent on weakly documented presumptions about
      -- how unwinding works (as is the invoke instruction)
      withContext $ \context -> do
        let triple = "x86_64-apple-darwin"
        (target, _) <- failInIO $ lookupTarget Nothing triple
        withTargetOptions $ \targetOptions -> do
          withTargetMachine target triple "" Set.empty targetOptions
                            R.Default CM.Default CGO.Default $ \tm -> do
            withPassManager (defaultPassSetSpec { transforms = [LowerInvoke False], targetMachine = Just tm}) $ \passManager -> do
              let astIn = 
                    Module "<string>" Nothing Nothing [
                     GlobalDefinition $ Function L.External V.Default CC.C [] (IntegerType 32) (Name "foo") ([
                           Parameter (IntegerType 32) (Name "x") []
                          ],False) [] Nothing 0 [
                            BasicBlock (Name "here") [
                            ] (
                              Do $ Ret (Just (ConstantOperand (C.Int 32 0))) []
                            )
                          ]
                     ] 
              astOut <- withModuleFromAST' context astIn $ \mIn -> do
                runPassManager passManager mIn
                moduleAST mIn
              astOut @?= Module "<string>" Nothing Nothing [
                      GlobalDefinition $ Function L.External V.Default CC.C [] (IntegerType 32) (Name "foo") ([
                          Parameter (IntegerType 32) (Name "x") []
                        ],False) [] Nothing 0 [
                         BasicBlock (Name "here") [
                         ] (
                           Do $ Ret (Just (ConstantOperand (C.Int 32 0))) []
                         )
                        ],
                       GlobalDefinition $ Function L.External V.Default CC.C [] VoidType (Name "abort") ([],False)
                               [] Nothing 0 []
                      ]
   ]
 ]
