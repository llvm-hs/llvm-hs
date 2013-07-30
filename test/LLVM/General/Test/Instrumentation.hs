module LLVM.General.Test.Instrumentation where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit

import LLVM.General.Test.Support

import Control.Monad.Error
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

instrument :: PassManagerSpecification s => s -> A.Module -> IO A.Module
instrument s m = withContext $ \context -> withModuleFromAST' context m $ \mIn' -> do
  withPassManager s $ \pm -> runPassManager pm mIn'
  moduleAST mIn'

ast = do
 dl <- withDefaultTargetMachine getTargetMachineDataLayout
 return $ Module "<string>" (Just dl) Nothing [
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

tests = testGroup "Instrumentation" [
  testCase "print" $ do
    Right ast <- runErrorT ast
    s <- withContext $ \context -> withModuleFromAST' context ast moduleString
    putStrLn s,
  testGroup "basic" [
    testCase n $ do
      Right ast <- runErrorT ast
      Module { moduleDefinitions = defs } <- instrument [p] ast
      [ n | GlobalDefinition d <- defs, Name n <- return (G.name d) ]
       @?= r
    | (n,p,r) <- [
     (
       "EdgeProfiler", EdgeProfiler,
       ["EdgeProfCounters","foo","main","llvm_start_edge_profiling"]
     ), (
       "OptimalEdgeProfiler", OptimalEdgeProfiler, 
       ["OptEdgeProfCounters","foo","main","llvm_start_opt_edge_profiling"]
     ), (
       "PathProfiler", PathProfiler,
       [
        "functionPathTable",
        "foo",
        "main",
        "llvm_increment_path_count",
        "llvm_decrement_path_count",
        "llvm_start_path_profiling"
       ]
     ), (
       "GCOVProfiler", defaultGCOVProfiler,
       ["foo", "main"]
     ), {- this input causes opt -asan to segfault -} (
       "AddressSanitizer",
       defaultAddressSanitizer,
       []
     ), (
       "AddressSanitizerModule", defaultAddressSanitizerModule,
       ["foo", "main"]
     ), {- (
       "MemorySanitizer", defaultMemorySanitizer,
       []
     ), -} (
       "ThreadSanitizer", defaultThreadSanitizer,
       []
     ){-, (
       "BoundsChecking", BoundsChecking,
       []
     ) -}
    ]
   ]
 ]
