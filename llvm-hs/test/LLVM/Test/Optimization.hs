{-# LANGUAGE OverloadedStrings #-}
module LLVM.Test.Optimization where

import Test.Tasty
import Test.Tasty.HUnit

import LLVM.Test.Support

import Data.Functor
import Data.Monoid
import qualified Data.Set as Set
import qualified Data.Map as Map

import LLVM.Module
import LLVM.Context
import LLVM.PassManager
import qualified LLVM.Transforms as T
import LLVM.Target

import LLVM.AST as A
import LLVM.AST.Type as A.T
import LLVM.AST.Name
import LLVM.AST.AddrSpace
import LLVM.AST.DataLayout
import qualified LLVM.AST.IntegerPredicate as IPred
import qualified LLVM.AST.Linkage as L
import qualified LLVM.AST.Visibility as V
import qualified LLVM.AST.CallingConvention as CC
import qualified LLVM.AST.Attribute as A
import qualified LLVM.AST.Global as G
import qualified LLVM.AST.Constant as C

import qualified LLVM.Relocation as R
import qualified LLVM.CodeModel as CM
import qualified LLVM.CodeGenOpt as CGO

handAST = 
  Module "<string>" "<string>" Nothing Nothing [
      GlobalDefinition $ functionDefaults {
        G.returnType = i32,
        G.name = Name "foo",
        G.parameters = ([Parameter i32 (Name "x") []], False),
        G.functionAttributes = [Left (A.GroupID 0)],
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
       },
      FunctionAttributes (A.GroupID 0) [A.NoUnwind, A.ReadNone, A.UWTable]
     ]

isVectory :: A.Module -> Assertion
isVectory Module { moduleDefinitions = ds } =
  (@? "Module is not vectory") $ not $ null [ i 
    | GlobalDefinition (Function { G.basicBlocks = b }) <- ds,
      BasicBlock _ is _ <- b,
      _ := i@(ExtractElement {}) <- is
   ]

optimize :: PassSetSpec -> A.Module -> IO A.Module
optimize pss m = withContext $ \context -> withModuleFromAST context m $ \mIn' -> do
  withPassManager pss $ \pm -> runPassManager pm mIn'
  moduleAST mIn'

tests = testGroup "Optimization" [
  testCase "curated" $ do
    mOut <- optimize defaultCuratedPassSetSpec handAST

    mOut @?= Module "<string>" "<string>" Nothing Nothing [
      GlobalDefinition $ functionDefaults {
        G.returnType = i32,
         G.name = Name "foo",
         G.parameters = ([Parameter i32 (Name "x") []], False),
         G.functionAttributes = [Left (A.GroupID 0)],
         G.basicBlocks = [
           BasicBlock (Name "here") [
              ] (
              Do $ Ret (Just (ConstantOperand (C.Int 32 0))) []
            )
          ]
        },
      FunctionAttributes (A.GroupID 0) [A.NoRecurse, A.NoUnwind, A.ReadNone, A.UWTable]
      ],

  testGroup "individual" [
    testCase "ConstantPropagation" $ do
      mOut <- optimize defaultPassSetSpec { transforms = [T.ConstantPropagation] } handAST

      mOut @?= Module "<string>" "<string>" Nothing Nothing [
        GlobalDefinition $ functionDefaults {
          G.returnType = i32,
          G.name = Name "foo",
          G.parameters = ([Parameter i32 (Name "x") []], False),
          G.functionAttributes = [Left (A.GroupID 0)],
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
         },
        FunctionAttributes (A.GroupID 0) [A.NoUnwind, A.ReadNone, A.UWTable]
       ],

    testCase "SLPVectorization" $ do
      let
        fadd op0 op1 =
          FAdd { fastMathFlags = noFastMathFlags, operand0 = op0, operand1 = op1, metadata = [] }
        doubleVec = VectorType 2 double
        constInt i = ConstantOperand (C.Int {C.integerBits = 32, C.integerValue = i})
        undef = ConstantOperand (C.Undef doubleVec)
        extractElement vec index' =
          ExtractElement { vector = vec, index = index', metadata = [] }
        insertElement vec el i =
          InsertElement { vector = vec, element = el, index = i, metadata = [] }
        mIn = Module "<string>" "<string>" Nothing Nothing [
          GlobalDefinition $ functionDefaults {
           G.returnType = doubleVec,
            G.name = Name "buildVector_add_2f64",
            G.parameters = ([
              Parameter doubleVec n []
                | n <- [ "a", "b" ]
             ], False),
            G.basicBlocks = [
              BasicBlock (UnName 0)
                ["a0" := extractElement (LocalReference doubleVec "a") (constInt 0),
                 "a1" := extractElement (LocalReference doubleVec "a") (constInt 1),
                 "b0" := extractElement (LocalReference doubleVec "b") (constInt 0),
                 "b1" := extractElement (LocalReference doubleVec "b") (constInt 1),
                 "c0" := fadd (LocalReference double "a0") (LocalReference double "b0"),
                 "c1" := fadd (LocalReference double "a1") (LocalReference double "b1"),
                 "r0" := insertElement undef (LocalReference double "c0") (constInt 0),
                 "r1" := insertElement (LocalReference doubleVec "r0") (LocalReference double "c1") (constInt 1)
                ]
                (Do (Ret (Just (LocalReference doubleVec "r1")) []))
             ]
          }
         ]
      mOut <- optimize (defaultPassSetSpec {
                    transforms = [
                     T.SuperwordLevelParallelismVectorize,
                     T.InstructionCombining,
                     T.GlobalValueNumbering False
                    ] }) mIn
      mOut @?=
        Module "<string>" "<string>" Nothing Nothing [
            GlobalDefinition $ functionDefaults {
             G.returnType = doubleVec,
              G.name = Name "buildVector_add_2f64",
              G.parameters = ([
                Parameter doubleVec n []
                  | n <- [ "a", "b" ]
               ], False),
              G.basicBlocks = [
                BasicBlock (UnName 0)
                  [UnName 1 := fadd (LocalReference doubleVec "a") (LocalReference doubleVec "b")]
                  (Do (Ret (Just (LocalReference doubleVec (UnName 1))) []))
               ]
            }
           ],

    testCase "LoopVectorize" $ do
      let
        mIn = 
          Module {
            moduleName = "<string>",
            moduleSourceFileName = "<string>",
            moduleDataLayout = Just $ (defaultDataLayout BigEndian) { 
              typeLayouts = Map.singleton (VectorAlign, 128) (AlignmentInfo 128 128)
             },
            moduleTargetTriple = Just "x86_64",
            moduleDefinitions = [
              GlobalDefinition $ globalVariableDefaults {
                G.name = Name "a",
                G.linkage = L.Common,
                G.type' = A.T.ArrayType 2048 i32,
                G.initializer = Just (C.AggregateZero (A.T.ArrayType 2048 i32))
               },
              GlobalDefinition $ functionDefaults {
                G.returnType = A.T.void,
                G.name = Name "inc",
                G.functionAttributes = [Left (A.GroupID 0)],
                G.parameters = ([Parameter i32 (Name "n") []], False),
                G.basicBlocks = [
                  BasicBlock (UnName 0) [
                    UnName 1 := ICmp IPred.SGT (LocalReference i32 (Name "n")) (ConstantOperand (C.Int 32 0)) []
                   ] (Do $ CondBr (LocalReference i1 (UnName 1)) (Name ".lr.ph") (Name "._crit_edge") []),
                  BasicBlock (Name ".lr.ph") [
                    Name "indvars.iv" := Phi i64 [ 
                      (ConstantOperand (C.Int 64 0), UnName 0),
                      (LocalReference i64 (Name "indvars.iv.next"), Name ".lr.ph")
                     ] [],
                    UnName 2 := GetElementPtr True (ConstantOperand (C.GlobalReference (PointerType (A.T.ArrayType 2048 i32) (AddrSpace 0)) (Name "a"))) [
                      ConstantOperand (C.Int 64 0),
                      (LocalReference i64 (Name "indvars.iv"))
                     ] [],
                    UnName 3 := Load False (LocalReference (ptr i32) (UnName 2)) Nothing 4 [],
                    UnName 4 := Trunc (LocalReference i64 (Name "indvars.iv")) i32 [],
                    UnName 5 := Add True False (LocalReference i32 (UnName 3)) (LocalReference i32 (UnName 4)) [],
                    Do $ Store False (LocalReference (ptr i32) (UnName 2)) (LocalReference i32 (UnName 5)) Nothing 4 [],
                    Name "indvars.iv.next" := Add False False (LocalReference i64 (Name "indvars.iv")) (ConstantOperand (C.Int 64 1)) [],
                    Name "lftr.wideiv" := Trunc (LocalReference i64 (Name "indvars.iv.next")) i32 [],
                    Name "exitcond" := ICmp IPred.EQ (LocalReference i32 (Name "lftr.wideiv")) (LocalReference i32 (Name "n")) []
                   ] (Do $ CondBr (LocalReference i1 (Name "exitcond")) (Name "._crit_edge") (Name ".lr.ph") []),
                  BasicBlock (Name "._crit_edge") [
                   ] (Do $ Ret Nothing [])
                 ]
               },
              FunctionAttributes (A.GroupID 0) [A.NoUnwind, A.ReadNone, A.UWTable, A.StackProtect]
             ]
           }
      mOut <- do
        initializeAllTargets
        let triple = "x86_64"
        (target, _) <- lookupTarget Nothing triple
        withTargetOptions $ \targetOptions -> do
          withTargetMachine target triple "" Map.empty targetOptions R.Default CM.Default CGO.Default $ \tm -> do
            optimize (defaultPassSetSpec { 
                        transforms = [ T.defaultLoopVectorize ],
                        dataLayout = moduleDataLayout mIn,
                        targetMachine = Just tm
                      }) mIn
      isVectory mOut,

    testCase "LowerInvoke" $ do
      -- This test doesn't test much about what LowerInvoke does, just that it seems to work.
      -- The pass seems to be quite deeply dependent on weakly documented presumptions about
      -- how unwinding works (as is the invoke instruction)
      withContext $ \context -> do
        withPassManager (defaultPassSetSpec { transforms = [T.LowerInvoke] }) $ \passManager -> do
          let astIn = 
                Module "<string>" "<string>" Nothing Nothing [
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
          astOut <- withModuleFromAST context astIn $ \mIn -> do
            runPassManager passManager mIn
            moduleAST mIn
          astOut @?= Module "<string>" "<string>" Nothing Nothing [
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
   ]
 ]
