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
import LLVM.General.Diagnostic
import LLVM.General.Target
import LLVM.General.AST
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
           IntegerType 32,
           PointerType (NamedTypeReference (UnName 1)) (AddrSpace 0),
           PointerType (NamedTypeReference (UnName 0)) (AddrSpace 0)
          ]),
      TypeDefinition (UnName 1) Nothing,
      GlobalDefinition $ globalVariableDefaults {
        G.name = UnName 0,
        G.type' = IntegerType 32,
        G.initializer = Just (C.Int 32 1)
      },
      GlobalDefinition $ globalVariableDefaults {
        G.name = UnName 1,
        G.visibility = V.Protected,
        G.type' = IntegerType 32,
        G.addrSpace = AddrSpace 3,
        G.section = Just "foo"
      },
      GlobalDefinition $ globalVariableDefaults {
        G.name = UnName 2,
        G.hasUnnamedAddr = True,
        G.type' = IntegerType 8,
        G.initializer = Just (C.Int 8 2)
      },
      GlobalDefinition $ globalVariableDefaults {
        G.name = UnName 3,
        G.type' = NamedTypeReference (UnName 0)
      },
      GlobalDefinition $ globalVariableDefaults {
        G.name = UnName 4,
        G.type' = ArrayType (1 `shift` 32) (IntegerType 32)
      },
      GlobalDefinition $ globalVariableDefaults {
        G.name = Name ".argyle",
        G.type' = IntegerType 32,
        G.initializer = Just (C.Int 32 0),
        G.isThreadLocal = True
      },
      GlobalDefinition $ globalAliasDefaults {
        G.name = Name "three", 
        G.linkage = L.Private,
        G.type' = PointerType (IntegerType 32) (AddrSpace 3),
        G.aliasee = C.GlobalReference (UnName 1)
      },
      GlobalDefinition $ globalAliasDefaults {
        G.name = Name "two",
        G.type' = PointerType (IntegerType 32) (AddrSpace 3),
        G.aliasee = C.GlobalReference (Name "three")
      },
      GlobalDefinition $ functionDefaults {
        G.returnType = IntegerType 32,
        G.name = Name "bar",
        G.basicBlocks = [
          BasicBlock (UnName 0) [
           UnName 1 := Call {
             isTailCall = False,
             callingConvention = CC.C,
             returnAttributes = [A.ZeroExt],
             function = Right (ConstantOperand (C.GlobalReference (Name "foo"))),
             arguments = [
              (ConstantOperand (C.Int 32 1), [A.InReg]),
              (ConstantOperand (C.Int 8 4), [A.SignExt])
             ],
             functionAttributes = [A.NoUnwind, A.ReadNone, A.UWTable],
             metadata = []
           }
         ] (
           Do $ Ret (Just (LocalReference (UnName 1))) []
         )
        ]
      },
      GlobalDefinition $ functionDefaults {
        G.returnAttributes = [A.ZeroExt],
        G.returnType = IntegerType 32,
        G.name = Name "foo",
        G.parameters = ([
          Parameter (IntegerType 32) (Name "x") [A.InReg],
          Parameter (IntegerType 8) (Name "y") [A.SignExt]
         ], False),
        G.functionAttributes = [A.NoUnwind, A.ReadNone, A.UWTable],
        G.basicBlocks = [
          BasicBlock (UnName 0) [
           UnName 1 := Mul {
             nsw = True,
             nuw = False,
             operand0 = LocalReference (Name "x"),
             operand1 = LocalReference (Name "x"),
             metadata = []
           }
           ] (
             Do $ Br (Name "here") []
           ),
          BasicBlock (Name "here") [
           Name "go" := ICmp {
             iPredicate = IPred.EQ,
             operand0 = LocalReference (UnName 1),
             operand1 = LocalReference (Name "x"),
             metadata = []
           }
           ] (
              Do $ CondBr {
                condition = LocalReference (Name "go"),
                trueDest = Name "there",
                falseDest = Name "elsewhere",
                metadata' = []
              }
           ),
          BasicBlock (Name "there") [
           UnName 2 := Add {
             nsw = True,
             nuw = False,
             operand0 = LocalReference (UnName 1),
             operand1 = ConstantOperand (C.Int 32 3),
             metadata = []
           }
           ] (
             Do $ Br (Name "elsewhere") []
           ),
          BasicBlock (Name "elsewhere") [
           Name "r" := Phi {
             type' = IntegerType 32,
             incomingValues = [
               (ConstantOperand (C.Int 32 2), Name "there"),
               (ConstantOperand (C.Int 32 57), Name "here")
             ],
             metadata = []
           }
           ] (
             Do $ Ret (Just (LocalReference (Name "r"))) []
           )
         ]
        }
      ]

tests = testGroup "Module" [
  testGroup "withModuleFromString" [
    testCase "basic" $ withContext $ \context -> do
      z <- withModuleFromString' context handString (const $ return 0)
      z @?= 0,
    testCase "numbering" $ withContext $ \context -> do
      let s = "@0 = global i32 3\
              \define i32 @1(i32 %x) {\n\
              \  %1 = mul i32 %x, %x\n\
              \  %2 = add i32 %1, 3\n\
              \  ret i32 %2\n\
              \}\n"
      z <- withModuleFromString' context s (const $ return 0)
      z @?= 0
   ],

  testGroup "emit" [
    testCase "assemble" $ withContext $ \context -> do
      let s = "define i32 @main(i32 %argc, i8** %argv) {\n\
              \  ret i32 0\n\
              \}\n"
      a <- withModuleFromString' context s $ \m -> do
        (t, _) <- failInIO $ lookupTarget Nothing "x86_64-unknown-linux"
        withTargetOptions $ \to -> do
          withTargetMachine t "" "" Set.empty to R.Default CM.Default CGO.Default $ \tm -> do
            failInIO $ moduleAssembly tm m
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
    s <- withModuleFromString' context handString moduleString
    s @?= handString,

  testCase "moduleAST" $ withContext $ \context -> do
    ast <- withModuleFromString' context handString moduleAST
    ast @?= handAST,
    
  testCase "withModuleFromAST" $ withContext $ \context -> do
   s <- withModuleFromAST' context handAST moduleString
   s @?= handString,

  testCase "triple" $ withContext $ \context -> do
   let hAST = "; ModuleID = '<string>'\n\
              \target triple = \"x86_64-unknown-linux\"\n"
   ast <- withModuleFromString' context hAST moduleAST
   ast @?= defaultModule { moduleTargetTriple = Just "x86_64-unknown-linux" },

  testGroup "regression" [
    testCase "set flag on constant expr" $ withContext $ \context -> do
      let ast = Module "<string>" Nothing Nothing [
             GlobalDefinition $ functionDefaults {
               G.returnType = IntegerType 32,
               G.name = Name "foo",
               G.parameters = ([Parameter (IntegerType 32) (Name "x") []], False),
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
                    Do $ Ret (Just (LocalReference (UnName 1))) []
                  )
                ]
             }
           ]
      t <- withModuleFromAST' context ast $ \_ -> return True
      t @?= True,

    testCase "Phi node finishes" $ withContext $ \context -> do
      let ast = Module "<string>" Nothing Nothing [
            GlobalDefinition $ functionDefaults {
              G.returnType = IntegerType 32,
              G.name = Name "foo",
              G.parameters = ([Parameter (IntegerType 32) (Name "x") []], False),
              G.basicBlocks = [
                BasicBlock (UnName 0) [
                 UnName 1 := Mul {
                   nsw = True,
                   nuw = False,
                   operand0 = LocalReference (Name "x"),
                   operand1 = LocalReference (Name "x"),
                   metadata = []
                 }
                 ] (
                   Do $ Br (Name "here") []
                 ),
                BasicBlock (Name "here") [
                 UnName 2 := Phi (IntegerType 32) [ (ConstantOperand (C.Int 32 42), UnName 0) ] []
                 ] (
                   Do $ Br (Name "elsewhere") []
                 ),
                BasicBlock (Name "elsewhere") [             
                 ] (
                   Do $ Br (Name "there") []
                 ),
                BasicBlock (Name "there") [
                 ] (
                   Do $ Ret (Just (LocalReference (UnName 1))) []
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
                  G.returnType = IntegerType 32,
                  G.parameters = ([Parameter (IntegerType 32) (UnName 0) []], False),
                  G.basicBlocks = [
                   BasicBlock (UnName 1) [] (Do $ Switch (LocalReference $ UnName 0) (Name "end") cbps [])
                  ] ++ [
                   BasicBlock (UnName n) [] (Do $ Br (Name "end") []) | n <- ns
                  ] ++ [
                   BasicBlock (Name "end") [
                     Name "val" := Phi (IntegerType 32) vbps []
                   ] (
                     Do $ Ret (Just (LocalReference (Name "val"))) []
                   )
                  ]
                }
               ]
          s <- withModuleFromAST' context ast moduleString
          m <- withModuleFromString' context s moduleAST
          m @?= ast
   ],
        
  testGroup "failures" [
    testCase "bad block reference" $ withContext $ \context -> do
      let badAST = Module "<string>" Nothing Nothing [
            GlobalDefinition $ functionDefaults {
              G.returnType = IntegerType 32,
              G.name = Name "foo",
              G.parameters = ([Parameter (IntegerType 32) (Name "x") []], False),
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
                   Do $ Ret (Just (LocalReference (UnName 1))) []
                 )
               ]
             }
           ]
      t <- runErrorT $ withModuleFromAST context badAST $ \_ -> return True
      t @?= Left "reference to undefined block: Name \"not here\"",

    testCase "multiple" $ withContext $ \context -> do
      let badAST = Module "<string>" Nothing Nothing [
            GlobalDefinition $ functionDefaults {
              G.returnType = IntegerType 32,
              G.name = Name "foo",
              G.basicBlocks = [
                BasicBlock (UnName 0) [
                 UnName 1 := Mul {
                   nsw = False,
                   nuw = False,
                   operand0 = LocalReference (Name "unknown"),
                   operand1 = ConstantOperand (C.Int 32 1),
                   metadata = []
                 },
                 UnName 2 := Mul {
                   nsw = False,
                   nuw = False,
                   operand0 = LocalReference (Name "unknown2"),
                   operand1 = LocalReference (UnName 1),
                   metadata = []
                 }
                 ] (
                   Do $ Ret (Just (LocalReference (UnName 2))) []
                 )
               ]
             }
           ]
      t <- runErrorT $ withModuleFromAST context badAST $ \_ -> return True
      t @?= Left "reference to undefined local: Name \"unknown\""
   ]
 ]
