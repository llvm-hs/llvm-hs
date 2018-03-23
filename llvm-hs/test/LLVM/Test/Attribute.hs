{-# LANGUAGE OverloadedStrings #-}
module LLVM.Test.Attribute
  ( tests
  ) where

import Test.Tasty
import Test.Tasty.HUnit

import LLVM
import LLVM.AST hiding (DIFlag(..))
import LLVM.AST.CallingConvention (CallingConvention(C))
import LLVM.AST.Constant (Constant (Int, GlobalReference))
import LLVM.AST.FunctionAttribute
import LLVM.AST.Global (basicBlocks, name, returnType)
import LLVM.AST.Type (i32, ptr)
import LLVM.Context

import Data.List
import Prelude

moduleAst :: LLVM.AST.Module
moduleAst =
  defaultModule
  { moduleDefinitions =
      [ GlobalDefinition
          (functionDefaults
           { returnType = i32
           , name = "f"
           , basicBlocks =
               [ BasicBlock
                   (UnName 0)
                   []
                   (Do
                      (Ret
                       { returnOperand = Just (ConstantOperand (Int 32 42))
                       , metadata' = []
                       }))
               ]
           })
      , GlobalDefinition
          (functionDefaults
           { returnType = i32
           , name = "call_15"
           , basicBlocks =
               [ BasicBlock
                   (UnName 0)
                   [ "result" :=
                     Call
                     { tailCallKind = Nothing
                     , callingConvention = C
                     , returnAttributes = []
                     , function =
                         Right
                           (ConstantOperand
                              (GlobalReference
                                 (ptr
                                    (FunctionType
                                     { resultType = i32
                                     , argumentTypes = []
                                     , isVarArg = False
                                     }))
                                 ("f")))
                     , arguments = []
                     , functionAttributes = [Left (GroupID 0)]
                     , metadata = []
                     }
                   ]
                   (Do
                      (Ret
                       { returnOperand = Just (LocalReference i32 ("result"))
                       , metadata' = []
                       }))
               ]
           })
      , FunctionAttributes
          (GroupID 0)
          [ AlwaysInline
          , ArgMemOnly
          , Builtin
          , Cold
          , Convergent
          , InaccessibleMemOnly
          , InaccessibleMemOrArgMemOnly
          , InlineHint
          , JumpTable
          , MinimizeSize
          , Naked
          , NoBuiltin
          , NoDuplicate
          , NoImplicitFloat
          , NoInline
          , NoRecurse
          , NoRedZone
          , NoReturn
          , NoUnwind
          , NonLazyBind
          , OptimizeForSize
          , OptimizeNone
          , ReadNone
          , ReadOnly
          , ReturnsTwice
          , SafeStack
          , SanitizeAddress
          , SanitizeMemory
          , SanitizeThread
          , StackProtect
          , StackProtectReq
          , StackProtectStrong
          , UWTable
          , WriteOnly
          , AllocSize 8 (Just 16)
          , StackAlignment 8
          , StringAttribute "bar" "baz"
          , StringAttribute "foo" ""
          , StringAttribute "qux" ""
          ]
      ]
  }

moduleStr :: String
moduleStr =
  unlines
    [ "define i32 @f() {"
    , "    ret i32 42"
    , "}"
    , ""
    , "define i32 @call_15() {"
    , "    %result = call i32 @f() \"foo\" \"bar\"=\"baz\" #0 alignstack(8) allocsize(8) allocsize(8, 16) alwaysinline argmemonly builtin cold convergent inaccessiblemem_or_argmemonly inaccessiblememonly inlinehint jumptable minsize naked nobuiltin noduplicate noimplicitfloat noinline nonlazybind norecurse noredzone noreturn nounwind optnone optsize readnone readonly returns_twice safestack sanitize_address sanitize_memory sanitize_thread ssp sspreq sspstrong uwtable writeonly"
    , "    ret i32 %result"
    , "}"
    , ""
    , "attributes #0 = { \"qux\" }"
    ]

tests :: TestTree
tests =
  testGroup "Attributes"
    [ testCase "decoding of call attributes"  $ do
        withContext $ \ctx -> do
          withModuleFromLLVMAssembly ctx moduleStr $ \m -> do
            ast <- moduleAST m
            ast @?= moduleAst
    ]
