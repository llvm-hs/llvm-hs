module LLVM.General.Test.Module where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit

import LLVM.General.Test.Support

import Data.Bits
import Data.Functor

import LLVM.General.Context
import LLVM.General.Module
import LLVM.General.Diagnostic
import LLVM.General.AST
import LLVM.General.AST.AddrSpace
import qualified LLVM.General.AST.IntegerPredicate as IPred

import qualified LLVM.General.AST.Linkage as L
import qualified LLVM.General.AST.Visibility as V
import qualified LLVM.General.AST.CallingConvention as CC
import qualified LLVM.General.AST.Attribute as A
import qualified LLVM.General.AST.Global as G
import qualified LLVM.General.AST.Constant as C

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
      GlobalDefinition $ Function L.External V.Default CC.C [] (IntegerType 32) (Name "bar") ([],False) [] Nothing 0 [
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
      ],
      GlobalDefinition $ Function L.External V.Default CC.C [A.ZeroExt] (IntegerType 32) (Name "foo") ([
          Parameter (IntegerType 32) (Name "x") [A.InReg],
          Parameter (IntegerType 8) (Name "y") [A.SignExt]
         ],False)
         [A.NoUnwind, A.ReadNone, A.UWTable] Nothing 0 
       [
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

  testCase "handStringIsCanonical" $ withContext $ \context -> do
    s <- withModuleFromString' context handString moduleString
    s @?= handString,

  testCase "moduleAST" $ withContext $ \context -> do
    ast <- withModuleFromString' context handString moduleAST
    ast @?= handAST,
    
  testCase "withModuleFromAST" $ withContext $ \context -> do
   s <- withModuleFromAST context handAST moduleString
   s @?= Right handString,

  testCase "triple" $ withContext $ \context -> do
   let hAST = "; ModuleID = '<string>'\n\
              \target triple = \"x86_64-unknown-linux\"\n"
   ast <- withModuleFromString' context hAST moduleAST
   ast @?= defaultModule { moduleTargetTriple = Just "x86_64-unknown-linux" },

  testGroup "regression" [
    testCase "set flag on constant expr" $ withContext $ \context -> do
      let ast = Module "<string>" Nothing Nothing [
           GlobalDefinition $ Function L.External V.Default CC.C [] (IntegerType 32) (Name "foo") ([
               Parameter (IntegerType 32) (Name "x") []
              ],False)
              [] Nothing 0 
            [
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
           ]
      t <- withModuleFromAST context ast $ \_ -> return True
      t @?= Right True
   ],

  testGroup "failures" [
    testCase "bad block reference" $ withContext $ \context -> do
      let badAST = Module "<string>" Nothing Nothing [
           GlobalDefinition $ Function L.External V.Default CC.C [] (IntegerType 32) (Name "foo") ([
               Parameter (IntegerType 32) (Name "x") []
              ],False)
              [] Nothing 0 
            [
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
           ]
      t <- withModuleFromAST context badAST $ \_ -> return True
      t @?= Left "reference to undefined block: Name \"not here\""
   ]
 ]
