module LLVM.General.Test.Module where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit

import LLVM.General.Test.Support

import Control.Monad.Error
import Data.Bits
import Data.Word
import Data.Functor

import qualified Data.Set as Set

import LLVM.General.Context
import LLVM.General.Module
import LLVM.General.Analysis
import LLVM.General.Diagnostic
import LLVM.General.Target
import LLVM.General.AST
import LLVM.General.AST.Type as A.T
import LLVM.General.AST.AddrSpace
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

handString = "; ModuleID = '<string>'\n\
    \\n\
    \%0 = type { i32, %1*, %0* }\n\
    \%1 = type opaque\n\
    \\n\
    \@0 = global i32 1\n\
    \@1 = external protected addrspace(3) global i32, section \"foo\"\n\
    \@2 = unnamed_addr global i8 2\n\
    \@3 = external global %0\n\
    \@4 = external global [4294967296 x i32]\n\
    \@.argyle = thread_local global i32 0\n\
    \\n\
    \@three = alias private i32 addrspace(3)* @1\n\
    \@two = alias i32 addrspace(3)* @three\n\
    \\n\
    \define i32 @bar() {\n\
    \  %1 = call zeroext i32 @foo(i32 inreg 1, i8 signext 4) #0\n\
    \  ret i32 %1\n\
    \}\n\
    \\n\
    \; Function Attrs: nounwind readnone uwtable\n\
    \define zeroext i32 @foo(i32 inreg %x, i8 signext %y) #0 {\n\
    \  %1 = mul nsw i32 %x, %x\n\
    \  br label %here\n\
    \\n\
    \here:                                             ; preds = %0\n\
    \  %go = icmp eq i32 %1, %x\n\
    \  br i1 %go, label %there, label %elsewhere\n\
    \\n\
    \there:                                            ; preds = %here\n\
    \  %2 = add nsw i32 %1, 3\n\
    \  br label %elsewhere\n\
    \\n\
    \elsewhere:                                        ; preds = %there, %here\n\
    \  %r = phi i32 [ 2, %there ], [ 57, %here ]\n\
    \  ret i32 %r\n\
    \}\n\
    \\n\
    \attributes #0 = { nounwind readnone uwtable }\n"

handAST = Module "<string>" Nothing Nothing [
      TypeDefinition (UnName 0) (
         Just $ StructureType False [
           i32,
           ptr (NamedTypeReference (UnName 1)),
           ptr (NamedTypeReference (UnName 0))
          ]),
      TypeDefinition (UnName 1) Nothing,
      GlobalDefinition $ globalVariableDefaults {
        G.name = UnName 0,
        G.type' = i32,
        G.initializer = Just (C.Int 32 1)
      },
      GlobalDefinition $ globalVariableDefaults {
        G.name = UnName 1,
        G.visibility = V.Protected,
        G.type' = i32,
        G.addrSpace = AddrSpace 3,
        G.section = Just "foo"
      },
      GlobalDefinition $ globalVariableDefaults {
        G.name = UnName 2,
        G.hasUnnamedAddr = True,
        G.type' = i8,
        G.initializer = Just (C.Int 8 2)
      },
      GlobalDefinition $ globalVariableDefaults {
        G.name = UnName 3,
        G.type' = NamedTypeReference (UnName 0)
      },
      GlobalDefinition $ globalVariableDefaults {
        G.name = UnName 4,
        G.type' = ArrayType (1 `shift` 32) i32
      },
      GlobalDefinition $ globalVariableDefaults {
        G.name = Name ".argyle",
        G.type' = i32,
        G.initializer = Just (C.Int 32 0),
        G.isThreadLocal = True
      },
      GlobalDefinition $ globalAliasDefaults {
        G.name = Name "three", 
        G.linkage = L.Private,
        G.type' = PointerType i32 (AddrSpace 3),
        G.aliasee = C.GlobalReference (PointerType i32 (AddrSpace 3)) (UnName 1)
      },
      GlobalDefinition $ globalAliasDefaults {
        G.name = Name "two",
        G.type' = PointerType i32 (AddrSpace 3),
        G.aliasee = C.GlobalReference (PointerType i32 (AddrSpace 3)) (Name "three")
      },
      GlobalDefinition $ functionDefaults {
        G.returnType = i32,
        G.name = Name "bar",
        G.basicBlocks = [
          BasicBlock (UnName 0) [
           UnName 1 := Call {
             isTailCall = False,
             callingConvention = CC.C,
             returnAttributes = [A.ZeroExt],
             function = Right (ConstantOperand (C.GlobalReference (ptr (FunctionType i32 [i32, i8] False)) (Name "foo"))),
             arguments = [
              (ConstantOperand (C.Int 32 1), [A.InReg]),
              (ConstantOperand (C.Int 8 4), [A.SignExt])
             ],
             functionAttributes = [A.NoUnwind, A.ReadNone, A.UWTable],
             metadata = []
           }
         ] (
           Do $ Ret (Just (LocalReference i32 (UnName 1))) []
         )
        ]
      },
      GlobalDefinition $ functionDefaults {
        G.returnAttributes = [A.ZeroExt],
        G.returnType = i32,
        G.name = Name "foo",
        G.parameters = ([
          Parameter i32 (Name "x") [A.InReg],
          Parameter i8 (Name "y") [A.SignExt]
         ], False),
        G.functionAttributes = [A.NoUnwind, A.ReadNone, A.UWTable],
        G.basicBlocks = [
          BasicBlock (UnName 0) [
           UnName 1 := Mul {
             nsw = True,
             nuw = False,
             operand0 = LocalReference i32 (Name "x"),
             operand1 = LocalReference i32 (Name "x"),
             metadata = []
           }
           ] (
             Do $ Br (Name "here") []
           ),
          BasicBlock (Name "here") [
           Name "go" := ICmp {
             iPredicate = IPred.EQ,
             operand0 = LocalReference i32 (UnName 1),
             operand1 = LocalReference i32 (Name "x"),
             metadata = []
           }
           ] (
              Do $ CondBr {
                condition = LocalReference i1 (Name "go"),
                trueDest = Name "there",
                falseDest = Name "elsewhere",
                metadata' = []
              }
           ),
          BasicBlock (Name "there") [
           UnName 2 := Add {
             nsw = True,
             nuw = False,
             operand0 = LocalReference i32 (UnName 1),
             operand1 = ConstantOperand (C.Int 32 3),
             metadata = []
           }
           ] (
             Do $ Br (Name "elsewhere") []
           ),
          BasicBlock (Name "elsewhere") [
           Name "r" := Phi {
             type' = i32,
             incomingValues = [
               (ConstantOperand (C.Int 32 2), Name "there"),
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

tests = testGroup "Module" [
  testGroup "withModuleFromString" [
    testCase "basic" $ withContext $ \context -> do
      z <- withModuleFromLLVMAssembly' context handString (const $ return 0)
      z @?= 0,
    testCase "numbering" $ withContext $ \context -> do
      let s = "@0 = global i32 3\
              \define i32 @1(i32 %x) {\n\
              \  %1 = mul i32 %x, %x\n\
              \  %2 = add i32 %1, 3\n\
              \  ret i32 %2\n\
              \}\n"
      z <- withModuleFromLLVMAssembly' context s (const $ return 0)
      z @?= 0
   ],

  testGroup "emit" [
    testCase "assemble" $ withContext $ \context -> do
      let s = "define i32 @main(i32 %argc, i8** %argv) {\n\
              \  ret i32 0\n\
              \}\n"
      a <- withModuleFromLLVMAssembly' context s $ \m -> do
        (t, _) <- failInIO $ lookupTarget Nothing "x86_64-unknown-linux"
        withTargetOptions $ \to -> do
          withTargetMachine t "" "" Set.empty to R.Default CM.Default CGO.Default $ \tm -> do
            failInIO $ moduleTargetAssembly tm m
      a @?= "\t.file\t\"<string>\"\n\
            \\t.text\n\
            \\t.globl\tmain\n\
            \\t.align\t16, 0x90\n\
            \\t.type\tmain,@function\n\
            \main:\n\
            \\t.cfi_startproc\n\
            \\txorl\t%eax, %eax\n\
            \\tret\n\
            \.Ltmp0:\n\
            \\t.size\tmain, .Ltmp0-main\n\
            \\t.cfi_endproc\n\
            \\n\
            \\n\
            \\t.section\t\".note.GNU-stack\",\"\",@progbits\n"
   ],

  testCase "handStringIsCanonical" $ withContext $ \context -> do
    s <- withModuleFromLLVMAssembly' context handString moduleLLVMAssembly
    s @?= handString,

  testCase "moduleAST" $ withContext $ \context -> do
    ast <- withModuleFromLLVMAssembly' context handString moduleAST
    ast @?= handAST,
    
  testCase "withModuleFromAST" $ withContext $ \context -> do
    s <- withModuleFromAST' context handAST moduleLLVMAssembly
    s @?= handString,

  testCase "bitcode" $ withContext $ \context -> do
    bs <- withModuleFromAST' context handAST moduleBitcode
    s <- withModuleFromBitcode' context bs moduleLLVMAssembly
    s @?= handString,

  testCase "triple" $ withContext $ \context -> do
   let hAST = "; ModuleID = '<string>'\n\
              \target triple = \"x86_64-unknown-linux\"\n"
   ast <- withModuleFromLLVMAssembly' context hAST moduleAST
   ast @?= defaultModule { moduleTargetTriple = Just "x86_64-unknown-linux" },

  testGroup "regression" [
    testCase "minimal type info" $ withContext $ \context -> do
      let s = "; ModuleID = '<string>'\n\
              \\n\
              \define void @trouble() {\n\
              \entry:\n\
              \  ret void\n\
              \\n\
              \dead0:                                            ; preds = %dead1\n\
              \  %x0 = add i32 %x1, %x1\n\
              \  br label %dead1\n\
              \\n\
              \dead1:                                            ; preds = %dead0\n\
              \  %x1 = add i32 %x0, %x0\n\
              \  br label %dead0\n\
              \}\n"
          ast = Module "<string>" Nothing Nothing [
             GlobalDefinition $ functionDefaults {
                G.returnType = A.T.void,
                G.name = Name "trouble",
                G.basicBlocks = [
                 BasicBlock (Name "entry") [
                  ] (
                   Do $ Ret Nothing []
                  ),
                 BasicBlock (Name "dead0") [
                   Name "x0" := Add {
                     nsw = False,
                     nuw = False,
                     operand0 = LocalReference i32 (Name "x1"),
                     operand1 = LocalReference i32 (Name "x1"),
                     metadata = []
                   }
                  ] (
                   Do $ Br (Name "dead1") []
                  ),
                 BasicBlock (Name "dead1") [
                   Name "x1" := Add {
                     nsw = False,
                     nuw = False,
                     operand0 = LocalReference i32 (Name "x0"),
                     operand1 = LocalReference i32 (Name "x0"),
                     metadata = []
                   }
                  ] (
                   Do $ Br (Name "dead0") []
                  )
                 ]
              }
            ]
      strCheck ast s
      s' <- withContext $ \context -> withModuleFromAST' context ast $ runErrorT . verify
      s' @?= Right (),
    testCase "set flag on constant expr" $ withContext $ \context -> do
      let ast = Module "<string>" Nothing Nothing [
             GlobalDefinition $ functionDefaults {
               G.returnType = i32,
               G.name = Name "foo",
               G.parameters = ([Parameter i32 (Name "x") []], False),
               G.basicBlocks = [
                 BasicBlock (UnName 0) [
                  UnName 1 := Mul {
                    nsw = True,
                    nuw = False,
                    operand0 = ConstantOperand (C.Int 32 1),
                    operand1 = ConstantOperand (C.Int 32 1),
                    metadata = []
                  }
                  ] (
                    Do $ Br (Name "here") []
                  ),
                 BasicBlock (Name "here") [
                  ] (
                    Do $ Ret (Just (LocalReference i32 (UnName 1))) []
                  )
                ]
             }
           ]
      t <- withModuleFromAST' context ast $ \_ -> return True
      t @?= True,

    testCase "Phi node finishes" $ withContext $ \context -> do
      let ast = Module "<string>" Nothing Nothing [
            GlobalDefinition $ functionDefaults {
              G.returnType = i32,
              G.name = Name "foo",
              G.parameters = ([Parameter i32 (Name "x") []], False),
              G.basicBlocks = [
                BasicBlock (UnName 0) [
                 UnName 1 := Mul {
                   nsw = True,
                   nuw = False,
                   operand0 = LocalReference i32 (Name "x"),
                   operand1 = LocalReference i32 (Name "x"),
                   metadata = []
                 }
                 ] (
                   Do $ Br (Name "here") []
                 ),
                BasicBlock (Name "here") [
                 UnName 2 := Phi i32 [ (ConstantOperand (C.Int 32 42), UnName 0) ] []
                 ] (
                   Do $ Br (Name "elsewhere") []
                 ),
                BasicBlock (Name "elsewhere") [             
                 ] (
                   Do $ Br (Name "there") []
                 ),
                BasicBlock (Name "there") [
                 ] (
                   Do $ Ret (Just (LocalReference i32 (UnName 1))) []
                 )
               ]
             }
           ]
          s = "; ModuleID = '<string>'\n\
              \\n\
              \define i32 @foo(i32 %x) {\n\
              \  %1 = mul nsw i32 %x, %x\n\
              \  br label %here\n\
              \\n\
              \here:                                             ; preds = %0\n\
              \  %2 = phi i32 [ 42, %0 ]\n\
              \  br label %elsewhere\n\
              \\n\
              \elsewhere:                                        ; preds = %here\n\
              \  br label %there\n\
              \\n\
              \there:                                            ; preds = %elsewhere\n\
              \  ret i32 %1\n\
              \}\n"
      strCheck ast s,

      testCase "switchblock" $ do
        let count = 450
            start = 2 -- won't come back the same w/o start = 2
            ns = [start..count + start - 1]
            vbps = zip [ ConstantOperand (C.Int 32 i) | i <- [0..] ] [ UnName n | n <- (1:ns) ]
            cbps = zip [ C.Int 32 i | i <- [0..] ] [ UnName n | n <- ns ]

        withContext $ \context -> do
          let ast = Module "<string>" Nothing Nothing [
                GlobalDefinition $ functionDefaults {
                  G.name = Name "foo",
                  G.returnType = i32,
                  G.parameters = ([Parameter i32 (UnName 0) []], False),
                  G.basicBlocks = [
                   BasicBlock (UnName 1) [] (Do $ Switch (LocalReference i32 (UnName 0)) (Name "end") cbps [])
                  ] ++ [
                   BasicBlock (UnName n) [] (Do $ Br (Name "end") []) | n <- ns
                  ] ++ [
                   BasicBlock (Name "end") [
                     Name "val" := Phi i32 vbps []
                   ] (
                     Do $ Ret (Just (LocalReference i32 (Name "val"))) []
                   )
                  ]
                }
               ]
          s <- withModuleFromAST' context ast moduleLLVMAssembly
          m <- withModuleFromLLVMAssembly' context s moduleAST
          m @?= ast,

      testCase "struct constant" $ do
        let s = "; ModuleID = '<string>'\n\
                \\n\
                \%0 = type { i32 }\n\
                \\n\
                \@0 = constant %0 { i32 1 }, align 4\n"
            ast = Module "<string>" Nothing Nothing [
              TypeDefinition (UnName 0) (Just $ StructureType False [i32]),
              GlobalDefinition $ globalVariableDefaults {
                G.name = UnName 0,
                G.isConstant = True,
                G.type' = NamedTypeReference (UnName 0),
                G.initializer = Just $ C.Struct (Just $ UnName 0) False [ C.Int 32 1 ],
                G.alignment = 4
              }
             ]
        strCheck ast s
   ],
        
  testGroup "failures" [
    testCase "bad block reference" $ withContext $ \context -> do
      let badAST = Module "<string>" Nothing Nothing [
            GlobalDefinition $ functionDefaults {
              G.returnType = i32,
              G.name = Name "foo",
              G.parameters = ([Parameter i32 (Name "x") []], False),
              G.basicBlocks = [
                BasicBlock (UnName 0) [
                 UnName 1 := Mul {
                   nsw = True,
                   nuw = False,
                   operand0 = ConstantOperand (C.Int 32 1),
                   operand1 = ConstantOperand (C.Int 32 1),
                   metadata = []
                 }
                 ] (
                   Do $ Br (Name "not here") []
                 ),
                BasicBlock (Name "here") [
                 ] (
                   Do $ Ret (Just (LocalReference i32 (UnName 1))) []
                 )
               ]
             }
           ]
      t <- runErrorT $ withModuleFromAST context badAST $ \_ -> return True
      t @?= Left "reference to undefined block: Name \"not here\"",

    testCase "multiple" $ withContext $ \context -> do
      let badAST = Module "<string>" Nothing Nothing [
            GlobalDefinition $ functionDefaults {
              G.returnType = i32,
              G.name = Name "foo",
              G.basicBlocks = [
                BasicBlock (UnName 0) [
                 UnName 1 := Mul {
                   nsw = False,
                   nuw = False,
                   operand0 = LocalReference i32 (Name "unknown"),
                   operand1 = ConstantOperand (C.Int 32 1),
                   metadata = []
                 },
                 UnName 2 := Mul {
                   nsw = False,
                   nuw = False,
                   operand0 = LocalReference i32 (Name "unknown2"),
                   operand1 = LocalReference i32 (UnName 1),
                   metadata = []
                 }
                 ] (
                   Do $ Ret (Just (LocalReference i32 (UnName 2))) []
                 )
               ]
             }
           ]
      t <- runErrorT $ withModuleFromAST context badAST $ \_ -> return True
      t @?= Left "reference to undefined local: Name \"unknown\""
   ]
 ]
