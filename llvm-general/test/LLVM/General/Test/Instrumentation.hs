module LLVM.General.Test.Instrumentation where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit

import LLVM.General.Test.Support

import Control.Monad.Trans.Except 
import Control.Monad.IO.Class

import Data.Functor
import qualified Data.List as List
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

instrument :: PassSetSpec -> A.Module -> IO A.Module
instrument s m = withContext $ \context -> withModuleFromAST' context m $ \mIn' -> do
  withPassManager s $ \pm -> runPassManager pm mIn'
  moduleAST mIn'

ast = do
 dl <- withHostTargetMachine getTargetMachineDataLayout
 triple <- liftIO getDefaultTargetTriple
 return $ Module "<string>" "<string>" (Just dl) (Just triple) [
  GlobalDefinition $ functionDefaults {
    G.returnType = i32,
    G.name = Name "foo",
    G.parameters = ([Parameter i128 (Name "x") []],False),
    G.basicBlocks = [
      BasicBlock (UnName 0) [] (Do $ Br (Name "checkDone") []),
      BasicBlock (Name "checkDone") [
        UnName 1 := Phi {
         type' = i128,
         incomingValues = [
          (LocalReference i128 (Name "x"), UnName 0),
          (LocalReference i128 (Name "x'"), Name "even"),
          (LocalReference i128 (Name "x''"), Name "odd")
         ],
         metadata = []
        },
        Name "count" := Phi {
         type' = i32,
         incomingValues = [
          (ConstantOperand (C.Int 32 1), UnName 0),
          (LocalReference i32 (Name "count'"), Name "even"),
          (LocalReference i32 (Name "count'"), Name "odd")
         ],
         metadata = []
        },
        Name "count'" := Add {
         nsw = False,
         nuw = False,
         operand0 = LocalReference i32 (Name "count"),
         operand1 = ConstantOperand (C.Int 32 1),
         metadata = []
        },
        Name "is one" := ICmp {
         iPredicate = IPred.EQ,
         operand0 = LocalReference i128 (UnName 1),
         operand1 = ConstantOperand (C.Int 128 1),
         metadata = []
        }
      ] (
        Do $ CondBr (LocalReference i1 (Name "is one")) (Name "done") (Name "checkOdd") []
      ),
      BasicBlock (Name "checkOdd") [
        Name "is odd" := Trunc (LocalReference i128 (UnName 1)) i1 []
      ] (
       Do $ CondBr (LocalReference i1 (Name "is odd")) (Name "odd") (Name "even") []
      ),
      BasicBlock (Name "even") [
        Name "x'" := UDiv True (LocalReference i128 (UnName 1)) (ConstantOperand (C.Int 128 2)) []
      ] (
        Do $ Br (Name "checkDone") []
      ),
      BasicBlock (Name "odd") [
        UnName 2 := Mul False False (LocalReference i128 (UnName 1)) (ConstantOperand (C.Int 128 3)) [],
        Name "x''" := Add False False (LocalReference i128 (UnName 2)) (ConstantOperand (C.Int 128 1)) []
      ] (
        Do $ Br (Name "checkDone") []
      ),
      BasicBlock (Name "done") [
      ] (
        Do $ Ret (Just (LocalReference i32 (Name "count'"))) []
      )
     ]
   },
  GlobalDefinition $ functionDefaults {
    G.returnType = i32,
    G.name = Name "main",
    G.parameters = ([
      Parameter i32 (Name "argc") [],
      Parameter (ptr (ptr i8)) (Name "argv") []
     ],False),
    G.basicBlocks = [
      BasicBlock (UnName 0) [
        UnName 1 := Call {
          tailCallKind = Nothing,
          callingConvention = CC.C,
          returnAttributes = [],
          function = Right (ConstantOperand (C.GlobalReference (FunctionType i32 [i32, ptr (ptr i8)] False) (Name "foo"))),
          arguments = [
           (ConstantOperand (C.Int 128 9491828328), [])
          ],
          functionAttributes = [],
          metadata = []
        }
      ] (
        Do $ Ret (Just (LocalReference i32 (UnName 1))) []
      )
     ]
   }
  ]

tests = testGroup "Instrumentation" [
  testGroup "basic" [
    testCase n $ do
      triple <- getProcessTargetTriple 
      withTargetLibraryInfo triple $ \tli -> do
        Right dl <- runExceptT $ withHostTargetMachine getTargetMachineDataLayout
        Right ast <- runExceptT ast
        ast' <- instrument (defaultPassSetSpec { transforms = [p], dataLayout = Just dl, targetLibraryInfo = Just tli }) ast
        let names ast = [ n | GlobalDefinition d <- moduleDefinitions ast, Name n <- return (G.name d) ]
        (names ast') `List.intersect` (names ast) @?= names ast
    | (n,p) <- [
     ("GCOVProfiler", defaultGCOVProfiler),
{-
     ("AddressSanitizer", defaultAddressSanitizer),
     ("AddressSanitizerModule", defaultAddressSanitizerModule),
-}
     ("MemorySanitizer", defaultMemorySanitizer),
     ("ThreadSanitizer", defaultThreadSanitizer),
     ("BoundsChecking", BoundsChecking)--,
    ]
   ]
 ]
