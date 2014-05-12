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
import LLVM.General.AST.Type as A.T
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
      GlobalDefinition $ functionDefaults {
        G.returnType = i32,
        G.name = Name "foo",
        G.parameters = ([Parameter i32 (Name "x") []], False),
        G.functionAttributes = [A.NoUnwind, A.ReadNone, A.UWTable], 
        G.basicBlocks = [
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
             operand0 = LocalReference i32 (UnName 1),
             operand1 = ConstantOperand (C.Int 32 42),
             metadata = []
           }
           ] (
              Do $ CondBr {
                condition = LocalReference i1 (Name "go"),
                trueDest = Name "take",
                falseDest = Name "done",
                metadata' = []
              }
           ),
          BasicBlock (Name "take") [
           UnName 2 := Sub {
             nsw = False,
             nuw = False,
             operand0 = LocalReference i32 (Name "x"),
             operand1 = LocalReference i32 (Name "x"),
             metadata = []
           }
           ] (
             Do $ Br (Name "done") []
           ),
          BasicBlock (Name "done") [
           Name "r" := Phi {
             type' = i32,
             incomingValues = [
               (LocalReference i32 (UnName 2), Name "take"),
               (ConstantOperand (C.Int 32 57), Name "here")
             ],
             metadata = []
           }
           ] (
             Do $ Ret (Just (LocalReference i32 (Name "r"))) []
           )
         ]
       }
     ]

isVectory :: A.Module -> Assertion
isVectory Module { moduleDefinitions = ds } =
  (@? "Module is not vectory") $ not $ null [ i 
    | GlobalDefinition (Function { G.basicBlocks = b }) <- ds,
      BasicBlock _ is _ <- b,
      _ := i@(InsertElement {}) <- is
   ]

optimize :: PassSetSpec -> A.Module -> IO A.Module
optimize pss m = withContext $ \context -> withModuleFromAST' context m $ \mIn' -> do
  withPassManager pss $ \pm -> runPassManager pm mIn'
  moduleAST mIn'

tests = testGroup "Optimization" [
  testCase "curated" $ do
    mOut <- optimize defaultCuratedPassSetSpec handAST

    mOut @?= Module "<string>" Nothing Nothing [
      GlobalDefinition $ functionDefaults {
        G.returnType = i32,
         G.name = Name "foo",
         G.parameters = ([Parameter i32 (Name "x") []], False),
         G.functionAttributes = [A.NoUnwind, A.ReadNone, A.UWTable],
         G.basicBlocks = [
           BasicBlock (Name "here") [
              ] (
              Do $ Ret (Just (ConstantOperand (C.Int 32 0))) []
            )
          ]
        }
      ],

  testGroup "individual" [
    testCase "ConstantPropagation" $ do
      mOut <- optimize defaultPassSetSpec { transforms = [ConstantPropagation] } handAST

      mOut @?= Module "<string>" Nothing Nothing [
        GlobalDefinition $ functionDefaults {
          G.returnType = i32,
          G.name = Name "foo",
          G.parameters = ([Parameter i32 (Name "x") []], False),
          G.functionAttributes = [A.NoUnwind, A.ReadNone, A.UWTable],
          G.basicBlocks = [
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
               operand0 = LocalReference i32 (Name "x"),
               operand1 = LocalReference i32 (Name "x"),
               metadata = []
              }
            ] (
             Do $ Br (Name "done") []
            ),
            BasicBlock (Name "done") [
             Name "r" := Phi {
               type' = i32,
               incomingValues = [(LocalReference i32 (UnName 1), Name "take"),(ConstantOperand (C.Int 32 57), Name "here")],
               metadata = []
              }
            ] (
              Do $ Ret (Just (LocalReference i32 (Name "r"))) []
            )
           ]
         }
       ],

    testCase "BasicBlockVectorization" $ do
      let
        mIn = Module "<string>" Nothing Nothing [
          GlobalDefinition $ functionDefaults {
           G.returnType = double,
            G.name = Name "foo",
            G.parameters = ([
              Parameter double (Name (l ++ n)) []
                | l <- [ "a", "b" ], n <- ["1", "2"]
             ], False),
            G.basicBlocks = [
              BasicBlock (UnName 0) ([
                Name (l ++ n) := op NoFastMathFlags (LocalReference double (Name (o1 ++ n))) (LocalReference double (Name (o2 ++ n))) []
                | (l, op, o1, o2) <- [
                   ("x", FSub, "a", "b"),
                   ("y", FMul, "x", "a"),
                   ("z", FAdd, "y", "b")],
                  n <- ["1", "2"]
               ] ++ [
                Name "r" := FMul NoFastMathFlags (LocalReference double (Name "z1")) (LocalReference double (Name "z2")) []
              ]) (Do $ Ret (Just (LocalReference double (Name "r"))) [])
             ]
          }
         ]
      mOut <- optimize (defaultPassSetSpec {
                    transforms = [
                     defaultVectorizeBasicBlocks { requiredChainDepth = 3 },
                     InstructionCombining, 
                     GlobalValueNumbering False
                    ] }) mIn
      isVectory mOut,
      
    testCase "LoopVectorize" $ do
      let
        mIn = 
          Module {
            moduleName = "<string>",
            moduleDataLayout = Just $ defaultDataLayout { 
              typeLayouts = Map.singleton (VectorAlign, 128) (AlignmentInfo 128 Nothing)
             },
            moduleTargetTriple = Just "x86_64",
            moduleDefinitions = [
              GlobalDefinition $ functionDefaults {
                G.returnType = A.T.void,
                G.name = Name "foo",
                G.parameters = ([Parameter (ptr i32) (Name "x") []], False),
                G.basicBlocks = [
                  BasicBlock (UnName 0) [] (Do $ Br (UnName 1) []),
                  BasicBlock (UnName 1) [
                    Name "i.0" := Phi i32 [ 
                      (ConstantOperand (C.Int 32 0), UnName 0),
                      (LocalReference i32 (UnName 8), UnName 7)
                     ] [],
                    Name ".0" := Phi (ptr i32) [ 
                      (LocalReference (ptr i32) (Name "x"), UnName 0),
                      (LocalReference (ptr i32) (UnName 4), UnName 7)
                     ] [],
                    UnName 2 := ICmp IPred.SLT (LocalReference i32 (Name "i.0")) (ConstantOperand (C.Int 32 2048)) []
                   ] (Do $ CondBr (LocalReference i1 (UnName 2)) (UnName 3) (UnName 9) []),
                  BasicBlock (UnName 3) [
                    UnName 4 := GetElementPtr True (LocalReference (ptr i32) (Name ".0")) [ 
                      ConstantOperand (C.Int 32 1)
                     ] [],
                    UnName 5 := Load False (LocalReference (ptr i32) (Name ".0")) Nothing 4 [],
                    UnName 6 := Add True False (LocalReference i32 (UnName 5)) (ConstantOperand (C.Int 32 1)) [],
                    Do $ Store False (LocalReference (ptr i32) (Name ".0")) (LocalReference i32 (UnName 6)) Nothing 4 []  
                   ] (Do $ Br (UnName 7) []),
                  BasicBlock (UnName 7) [
                    UnName 8 := Add True False (LocalReference i32 (Name "i.0")) (ConstantOperand (C.Int 32 1)) []
                   ] (Do $ Br (UnName 1) []),
                  BasicBlock (UnName 9) [] (Do $ Ret Nothing [])
                 ]
               }
             ]
           }
      mOut <- do
        let triple = "x86_64"
        (target, _) <- failInIO $ lookupTarget Nothing triple
        withTargetOptions $ \targetOptions -> do
          withTargetMachine target triple "" Set.empty targetOptions R.Default CM.Default CGO.Default $ \tm -> do
            optimize (defaultPassSetSpec { 
                        transforms = [ LoopVectorize ],
                        dataLayout = moduleDataLayout mIn,
                        targetMachine = Just tm
                      }) mIn
      isVectory mOut,

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
                      GlobalDefinition $ functionDefaults {
                        G.returnType = i32,
                        G.name = Name "foo",
                        G.parameters = ([Parameter i32 (Name "x") []], False),
                        G.basicBlocks = [
                          BasicBlock (Name "here") [
                          ] (
                            Do $ Ret (Just (ConstantOperand (C.Int 32 0))) []
                          )
                         ]
                       }
                     ] 
              astOut <- withModuleFromAST' context astIn $ \mIn -> do
                runPassManager passManager mIn
                moduleAST mIn
              astOut @?= Module "<string>" Nothing Nothing [
                      GlobalDefinition $ functionDefaults {
                        G.returnType = i32,
                        G.name = Name "foo",
                        G.parameters = ([Parameter i32 (Name "x") []], False),
                        G.basicBlocks = [
                          BasicBlock (Name "here") [
                          ] (
                            Do $ Ret (Just (ConstantOperand (C.Int 32 0))) []
                          )
                        ]
                      },
                      GlobalDefinition $ functionDefaults {
                        G.returnType = A.T.void,
                        G.name = Name "abort"
                      }
                     ]
   ]
 ]
