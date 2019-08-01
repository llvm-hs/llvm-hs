{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module LLVM.Test.Instructions where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck (Arbitrary(..), (===), ioProperty, oneof, testProperty)

import LLVM.Test.Support

import Control.Monad
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as ByteString
import Data.Functor
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.Maybe
import Data.Monoid
import Foreign.Ptr
import Data.Word

import LLVM.Context
import LLVM.Module
import LLVM.Diagnostic
import LLVM.AST
import LLVM.AST.Type as A.T
import LLVM.AST.Name
import LLVM.AST.AddrSpace
import qualified LLVM.AST.IntegerPredicate as IPred
import qualified LLVM.AST.FloatingPointPredicate as FPPred
import qualified LLVM.AST.Linkage as L
import qualified LLVM.AST.Visibility as V
import qualified LLVM.AST.CallingConvention as CC
import qualified LLVM.AST.Attribute as A
import qualified LLVM.AST.Global as G
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.RMWOperation as RMWOp

import LLVM.Internal.Coding (decodeM, encodeM)
import qualified LLVM.Internal.FFI.LLVMCTypes as FFI

tests = testGroup "Instructions" [
  testGroup "regular" [
    testCase name $ do
      let mAST = Module "<string>" "<string>" Nothing Nothing [
            GlobalDefinition $ functionDefaults {
              G.returnType = A.T.void,
              G.name = UnName 0,
              G.parameters = ([Parameter t (UnName n) [] | (t,n) <- zip ts [0..]], False),
              G.basicBlocks = [
                BasicBlock (UnName 7) [
                  namedInstr
                 ] (
                  Do $ Ret Nothing []
                 )
               ]
            }
           ]
          mStr = "; ModuleID = '<string>'\n\
                 \source_filename = \"<string>\"\n\
                 \\n\
                 \define void @0(i32, float, i32*, i64, i1, <2 x i32>, { i32, i32 }) {\n\
                 \  " <> namedInstrS <> "\n\
                 \  ret void\n\
                 \}\n"
      strCheck mAST mStr
    | let ts = [
           i32,
           float,
           ptr i32,
           i64,
           i1,
           VectorType 2 i32,
           StructureType False [i32, i32]
           ],
      let a i = LocalReference (ts !! fromIntegral i) (UnName i),
      (name, namedInstr, namedInstrS :: ByteString) <- (
        [
         (name, UnName 8 := instr, "%8 = " <> instrS)
         | (name, instr, instrS) <- [
          ("add",
           Add {
             nsw = False,
             nuw = False,
             operand0 = a 0,
             operand1 = a 0,
             metadata = [] 
           },
           "add i32 %0, %0"),
          ("nsw",
           Add {
             nsw = True,
             nuw = False,
             operand0 = a 0,
             operand1 = a 0,
             metadata = [] 
           },
           "add nsw i32 %0, %0"),
          ("nuw",
           Add {
             nsw = False,
             nuw = True,
             operand0 = a 0,
             operand1 = a 0,
             metadata = [] 
           },
           "add nuw i32 %0, %0"),
          ("fadd",
           FAdd {
             fastMathFlags = noFastMathFlags,
             operand0 = a 1,
             operand1 = a 1,
             metadata = [] 
           },
           "fadd float %1, %1"),
          ("sub",
           Sub {
             nsw = False,
             nuw = False,
             operand0 = a 0,
             operand1 = a 0,
             metadata = [] 
           },
           "sub i32 %0, %0"),
          ("fsub",
           FSub {
             fastMathFlags = noFastMathFlags,
             operand0 = a 1,
             operand1 = a 1,
             metadata = [] 
           },
           "fsub float %1, %1"),
          ("mul",
           Mul {
             nsw = False,
             nuw = False,
             operand0 = a 0,
             operand1 = a 0,
             metadata = [] 
           },
           "mul i32 %0, %0"),
          ("fmul",
           FMul {
             fastMathFlags = noFastMathFlags,
             operand0 = a 1,
             operand1 = a 1,
             metadata = [] 
           },
           "fmul float %1, %1"),
          ("udiv",
           UDiv {
             exact = False,
             operand0 = a 0,
             operand1 = a 0,
             metadata = [] 
           },
           "udiv i32 %0, %0"),
          ("exact",
           UDiv {
             exact = True,
             operand0 = a 0,
             operand1 = a 0,
             metadata = [] 
           },
           "udiv exact i32 %0, %0"),
          ("sdiv",
           SDiv {
             exact = False,
             operand0 = a 0,
             operand1 = a 0,
             metadata = [] 
           },
           "sdiv i32 %0, %0"),
          ("fdiv",
           FDiv {
             fastMathFlags = noFastMathFlags,
             operand0 = a 1,
             operand1 = a 1,
             metadata = [] 
           },
           "fdiv float %1, %1"),
          ("urem",
           URem {
             operand0 = a 0,
             operand1 = a 0,
             metadata = [] 
           },
           "urem i32 %0, %0"),
          ("srem",
           SRem {
             operand0 = a 0,
             operand1 = a 0,
             metadata = [] 
           },
           "srem i32 %0, %0"),
          ("frem",
           FRem {
             fastMathFlags = noFastMathFlags,
             operand0 = a 1,
             operand1 = a 1,
             metadata = [] 
           },
           "frem float %1, %1"),
          ("frem fast",
           FRem {
             fastMathFlags = FastMathFlags {
                 allowReassoc = True,
                 noNaNs = True,
                 noInfs = True,
                 noSignedZeros = True,
                 allowReciprocal = True,
                 allowContract = True,
                 approxFunc = True
             },
             operand0 = a 1,
             operand1 = a 1,
             metadata = [] 
           },
           "frem fast float %1, %1"),
          ("shl",
           Shl {
             nsw = False,
             nuw = False,
             operand0 = a 0,
             operand1 = a 0,
             metadata = [] 
           },
           "shl i32 %0, %0"),
          ("ashr",
           AShr {
             exact = False,
             operand0 = a 0,
             operand1 = a 0,
             metadata = [] 
           },
           "ashr i32 %0, %0"),
          ("lshr",
           LShr {
             exact = False,
             operand0 = a 0,
             operand1 = a 0,
             metadata = [] 
           },
           "lshr i32 %0, %0"),
          ("and",
           And {
             operand0 = a 0,
             operand1 = a 0,
             metadata = [] 
           },
           "and i32 %0, %0"),
          ("or",
           Or {
             operand0 = a 0,
             operand1 = a 0,
             metadata = [] 
           },
           "or i32 %0, %0"),
          ("xor",
           Xor {
             operand0 = a 0,
             operand1 = a 0,
             metadata = [] 
           },
           "xor i32 %0, %0"),
          ("alloca",
           Alloca {
             allocatedType = i32,
             numElements = Nothing,
             alignment = 0,
             metadata = [] 
           },
           "alloca i32"),
          ("alloca tricky",
           Alloca {
             allocatedType = IntegerType 7,
             numElements = Just (ConstantOperand (C.Int 32 2)),
             alignment = 128,
             metadata = [] 
           },
           "alloca i7, i32 2, align 128"),
          ("load",
           Load {
             volatile = False,
             address = a 2,
             maybeAtomicity = Nothing,
             alignment = 0,
             metadata = [] 
           },
           "load i32, i32* %2"),
          ("volatile",
           Load {
             volatile = True,
             address = a 2,
             maybeAtomicity = Nothing,
             alignment = 0,
             metadata = [] 
           },
           "load volatile i32, i32* %2"),
          ("acquire",
           Load {
             volatile = False,
             address = a 2,
             maybeAtomicity = Just (System, Acquire),
             alignment = 1,
             metadata = [] 
           },
           "load atomic i32, i32* %2 acquire, align 1"),
          ("singlethread",
           Load {
             volatile = False,
             address = a 2,
             maybeAtomicity = Just (SingleThread, Monotonic),
             alignment = 1,
             metadata = [] 
           },
           "load atomic i32, i32* %2 syncscope(\"singlethread\") monotonic, align 1"),
          ("GEP",
           GetElementPtr {
             inBounds = False,
             address = a 2,
             indices = [ a 0 ],
             metadata = [] 
           },
           "getelementptr i32, i32* %2, i32 %0"),
          ("inBounds",
           GetElementPtr {
             inBounds = True,
             address = a 2,
             indices = [ a 0 ],
             metadata = [] 
           },
           "getelementptr inbounds i32, i32* %2, i32 %0"),
          ("cmpxchg",
           CmpXchg {
             volatile = False,
             address = a 2,
             expected = a 0,
             replacement = a 0,
             atomicity = (System, Monotonic),
             failureMemoryOrdering = Monotonic,
             metadata = [] 
           },
           "cmpxchg i32* %2, i32 %0, i32 %0 monotonic monotonic"),
          ("atomicrmw",
           AtomicRMW {
             volatile = False,
             rmwOperation = RMWOp.UMax,
             address = a 2,
             value = a 0,
             atomicity = (System, Release),
             metadata = []
           },
           "atomicrmw umax i32* %2, i32 %0 release"),

          ("trunc",
           Trunc {
             operand0 = a 0,
             type' = i16,
             metadata = [] 
           },
           "trunc i32 %0 to i16"),
          ("zext",
           ZExt {
             operand0 = a 0,
             type' = i64,
             metadata = [] 
           },
           "zext i32 %0 to i64"),
          ("sext",
           SExt {
             operand0 = a 0,
             type' = i64,
             metadata = [] 
           },
           "sext i32 %0 to i64"),
          ("fptoui",
           FPToUI {
             operand0 = a 1,
             type' = i64,
             metadata = [] 
           },
           "fptoui float %1 to i64"),
          ("fptosi",
           FPToSI {
             operand0 = a 1,
             type' = i64,
             metadata = [] 
           },
           "fptosi float %1 to i64"),
          ("uitofp",
           UIToFP {
             operand0 = a 0,
             type' = float,
             metadata = [] 
           },
           "uitofp i32 %0 to float"),
          ("sitofp",
           SIToFP {
             operand0 = a 0,
             type' = float,
             metadata = [] 
           },
           "sitofp i32 %0 to float"),
          ("fptrunc",
           FPTrunc {
             operand0 = a 1,
             type' = half,
             metadata = [] 
           },
           "fptrunc float %1 to half"),
          ("fpext",
           FPExt {
             operand0 = a 1,
             type' = double,
             metadata = [] 
           },
           "fpext float %1 to double"),
          ("ptrtoint",
           PtrToInt {
             operand0 = a 2,
             type' = i32,
             metadata = [] 
           },
           "ptrtoint i32* %2 to i32"),
          ("inttoptr",
           IntToPtr {
             operand0 = a 0,
             type' = ptr i32,
             metadata = [] 
           },
           "inttoptr i32 %0 to i32*"),
          ("bitcast",
           BitCast {
             operand0 = a 0,
             type' = float,
             metadata = [] 
           },
           "bitcast i32 %0 to float"),
          ("addrspacecast",
           AddrSpaceCast {
             operand0 = a 2,
             type' = PointerType i32 (AddrSpace 2),
             metadata = [] 
           },
           "addrspacecast i32* %2 to i32 addrspace(2)*"),
          ("select",
           Select {
             condition' = a 4,
             trueValue = a 0,
             falseValue = a 0,
             metadata = []
           },
           "select i1 %4, i32 %0, i32 %0"),
          ("vaarg",
           VAArg {
             argList = a 2,
             type' = i16,
             metadata = []
           },
           "va_arg i32* %2, i16"),
          ("extractelement",
           ExtractElement {
             vector = a 5,
             index = a 0,
             metadata = []
           },
           "extractelement <2 x i32> %5, i32 %0"),
          ("insertelement",
           InsertElement {
             vector = a 5,
             element = a 0,
             index = a 0,
             metadata = []
           },
           "insertelement <2 x i32> %5, i32 %0, i32 %0"),
          ("shufflevector",
           ShuffleVector {
             operand0 = a 5,
             operand1 = a 5,
             mask = C.Vector [ C.Int 32 p | p <- [0..1] ],
             metadata = []
           },
           "shufflevector <2 x i32> %5, <2 x i32> %5, <2 x i32> <i32 0, i32 1>"),
          ("extractvalue",
           ExtractValue {
             aggregate = a 6,
             indices' = [0],
             metadata = []
           },
           "extractvalue { i32, i32 } %6, 0"),
          ("insertvalue",
           InsertValue {
             aggregate = a 6,
             element = a 0,
             indices' = [0],
             metadata = []
           },
           "insertvalue { i32, i32 } %6, i32 %0, 0")
         ] ++ [
          ("landingpad-" ++ n,
           LandingPad {
             type' = StructureType False [
                ptr i8,
                i32
               ],
             cleanup = cp,
             clauses = cls,
             metadata = []
           },
           "landingpad { i8*, i32 }" <> s)
          | (clsn,cls,clss) <- [
           ("catch",
            [Catch (C.Null (ptr i8))],
            "\n          catch i8* null"),
           ("filter",
            [Filter (C.AggregateZero (ArrayType 1 (ptr i8)))],
            "\n          filter [1 x i8*] zeroinitializer")
          ],
          (cpn, cp, cps) <- [ ("-cleanup", True, "\n          cleanup"), ("", False, "") ],
          let s = cps <> clss
              n = clsn <> cpn
         ] ++ [
          ("icmp-" ++ ByteString.unpack ps,
           ICmp { iPredicate = p, operand0 = a 0, operand1 = a 0, metadata = [] },
           "icmp " <> ps <> " i32 %0, %0")
           | (ps, p) <- [
           ("eq", IPred.EQ),
           ("ne", IPred.NE),
           ("ugt", IPred.UGT),
           ("uge", IPred.UGE),
           ("ult", IPred.ULT),
           ("ule", IPred.ULE),
           ("sgt", IPred.SGT),
           ("sge", IPred.SGE),
           ("slt", IPred.SLT),
           ("sle", IPred.SLE)
          ]
         ] ++ [
          ("fcmp-" ++ ByteString.unpack ps,
           FCmp { fpPredicate = p, operand0 = a 1, operand1 = a 1, metadata = [] },
           "fcmp " <> ps <> " float %1, %1")
           | (ps, p) <- [
           ("false", FPPred.False),
           ("oeq", FPPred.OEQ),
           ("ogt", FPPred.OGT),
           ("oge", FPPred.OGE),
           ("olt", FPPred.OLT),
           ("ole", FPPred.OLE),
           ("one", FPPred.ONE),
           ("ord", FPPred.ORD),
           ("uno", FPPred.UNO),
           ("ueq", FPPred.UEQ),
           ("ugt", FPPred.UGT),
           ("uge", FPPred.UGE),
           ("ult", FPPred.ULT),
           ("ule", FPPred.ULE),
           ("une", FPPred.UNE),
           ("true", FPPred.True)
          ]
         ]
        ] ++ [
         ("store",
          Do $ Store {
            volatile = False,
            address = a 2,
            value = a 0,
            maybeAtomicity = Nothing,
            alignment = 0,
            metadata = []
          },
          "store i32 %0, i32* %2"),
         ("fence",
          Do $ Fence {
            atomicity = (System, Acquire),
            metadata = []
          },
          "fence acquire"),
          ("call",
           Do $ Call {
             tailCallKind = Nothing,
             callingConvention = CC.C,
             returnAttributes = [],
             function = Right (ConstantOperand (C.GlobalReference (ptr (FunctionType A.T.void ts False)) (UnName 0))),
             arguments = [ (a i, []) | i <- [0..6] ],
             functionAttributes = [],
             metadata = []
           },
           "call void @0(i32 %0, float %1, i32* %2, i64 %3, i1 %4, <2 x i32> %5, { i32, i32 } %6)")
        ]
      )
   ],
  testCase "GEP inBounds constant" $ do
    let mAST = Module "<string>" "<string>" Nothing Nothing [
          GlobalDefinition $ globalVariableDefaults {
            G.name = Name "fortytwo",
            G.type' = i32,
            G.isConstant = True,
            G.initializer = Just $ C.Int 32 42
          },
          GlobalDefinition $ functionDefaults {
            G.returnType = i32,
            G.name = UnName 0,
            G.basicBlocks = [
              BasicBlock (UnName 1) [
                UnName 2 := GetElementPtr {
                  inBounds = True,
                  address = ConstantOperand (C.GlobalReference (ptr i32) (Name "fortytwo")),
                  indices = [ ConstantOperand (C.Int 32 0) ],
                  metadata = []
                },
                UnName 3 := Load {
                  volatile = False,
                  address = LocalReference (ptr i32) (UnName 2),
                  maybeAtomicity = Nothing,
                  alignment = 1,
                  metadata = []
                }
              ] (
                Do $ Ret (Just (LocalReference i32 (UnName 3))) []
              )
             ]
           }
          ]
        mStr = "; ModuleID = '<string>'\n\
               \source_filename = \"<string>\"\n\
               \\n\
               \@fortytwo = constant i32 42\n\
               \\n\
               \define i32 @0() {\n\
               \  %1 = getelementptr inbounds i32, i32* @fortytwo, i32 0\n\
               \  %2 = load i32, i32* %1, align 1\n\
               \  ret i32 %2\n\
               \}\n"
    s <- withContext $ \context -> withModuleFromAST context mAST moduleLLVMAssembly
    s @?= mStr,
    
  testGroup "terminators" [
    testCase name $ strCheck mAST mStr
    | (name, mAST, mStr) <- [
     (
       "ret",
       Module "<string>" "<string>" Nothing Nothing [
        GlobalDefinition $ functionDefaults {
          G.returnType = A.T.void,
          G.name = UnName 0,
          G.basicBlocks = [
            BasicBlock (UnName 0) [
             ] (
              Do $ Ret Nothing []
             )
           ]
         }
        ],
       "; ModuleID = '<string>'\n\
       \source_filename = \"<string>\"\n\
       \\n\
       \define void @0() {\n\
       \  ret void\n\
       \}\n"
     ), (
       "br",
       Module "<string>" "<string>" Nothing Nothing [
        GlobalDefinition $ functionDefaults {
          G.returnType = A.T.void,
          G.name = UnName 0,
          G.basicBlocks = [
            BasicBlock (UnName 0) [] (
              Do $ Br (Name "foo") []
             ),
            BasicBlock (Name "foo") [] (
              Do $ Ret Nothing []
             )
           ]
         }
        ],
       "; ModuleID = '<string>'\n\
       \source_filename = \"<string>\"\n\
       \\n\
       \define void @0() {\n\
       \  br label %foo\n\
       \\n\
       \foo:                                              ; preds = %0\n\
       \  ret void\n\
       \}\n"
     ), (
       "condbr",
       Module "<string>" "<string>" Nothing Nothing [
        GlobalDefinition $ functionDefaults {
          G.returnType = A.T.void,
          G.name = UnName 0,
          G.basicBlocks = [
            BasicBlock (Name "bar") [] (
              Do $ CondBr (ConstantOperand (C.Int 1 1)) (Name "foo") (Name "bar") []
             ),
            BasicBlock (Name "foo") [] (
              Do $ Ret Nothing []
             )
           ]
          }
        ],
       "; ModuleID = '<string>'\n\
       \source_filename = \"<string>\"\n\
       \\n\
       \define void @0() {\n\
       \bar:\n\
       \  br i1 true, label %foo, label %bar\n\
       \\n\
       \foo:                                              ; preds = %bar\n\
       \  ret void\n\
       \}\n"
     ), (
       "switch",
       Module "<string>" "<string>" Nothing Nothing [
         GlobalDefinition $ functionDefaults {
           G.returnType = A.T.void,
           G.name = UnName 0,
           G.basicBlocks = [
             BasicBlock (UnName 0) [] (
               Do $ Switch {
                 operand0' = ConstantOperand (C.Int 16 2),
                 defaultDest = Name "foo",
                 dests = [
                  (C.Int 16 0, UnName 0),
                  (C.Int 16 2, Name "foo"),
                  (C.Int 16 3, UnName 0)
                 ],
                 metadata' = []
              }
             ),
             BasicBlock (Name "foo") [] (
               Do $ Ret Nothing []
              )
            ]
          }
        ],
       "; ModuleID = '<string>'\n\
       \source_filename = \"<string>\"\n\
       \\n\
       \define void @0() {\n\
       \  switch i16 2, label %foo [\n\
       \    i16 0, label %0\n\
       \    i16 2, label %foo\n\
       \    i16 3, label %0\n\
       \  ]\n\
       \\n\
       \foo:                                              ; preds = %0, %0\n\
       \  ret void\n\
       \}\n"
     ), (
       "indirectbr",
       Module "<string>" "<string>" Nothing Nothing [
        GlobalDefinition $ globalVariableDefaults {
          G.name = UnName 0,
          G.type' = ptr i8,
          G.initializer = Just (C.BlockAddress (Name "foo") (UnName 2))
        },
        GlobalDefinition $ functionDefaults {
          G.returnType = A.T.void,
          G.name = Name "foo",
          G.basicBlocks = [
            BasicBlock (UnName 0) [
              UnName 1 := Load {
                       volatile = False,
                       address = ConstantOperand (C.GlobalReference (ptr (ptr i8)) (UnName 0)),
                       maybeAtomicity = Nothing,
                       alignment = 0,
                       metadata = [] 
                     }
            ] (
              Do $ IndirectBr {
                operand0' = LocalReference (ptr i8) (UnName 1),
                possibleDests = [UnName 2],
                metadata' = []
             }
            ),
            BasicBlock (UnName 2) [] (
              Do $ Ret Nothing []
             )
           ]
         }
        ],
--       \  indirectbr i8* null, [label %foo]\n\
       "; ModuleID = '<string>'\n\
       \source_filename = \"<string>\"\n\
       \\n\
       \@0 = global i8* blockaddress(@foo, %2)\n\
       \\n\
       \define void @foo() {\n\
       \  %1 = load i8*, i8** @0\n\
       \  indirectbr i8* %1, [label %2]\n\
       \\n\
       \2:                                                ; preds = %0\n\
       \  ret void\n\
       \}\n"
     ), (
       "invoke",
       Module "<string>" "<string>" Nothing Nothing [
        GlobalDefinition $ functionDefaults {
          G.returnType = A.T.void,
          G.name = UnName 0,
          G.personalityFunction = Just $ C.GlobalReference
            (ptr (FunctionType A.T.void [i32,i16] False))
            (UnName 0)
          ,
          G.parameters = ([
            Parameter i32 (UnName 0) [],
            Parameter i16 (UnName 1) []
           ], False),
          G.basicBlocks = [
            BasicBlock (UnName 2) [] (
              Do $ Invoke {
               callingConvention' = CC.C,
               returnAttributes' = [],
               function' = Right (ConstantOperand (C.GlobalReference (ptr (FunctionType A.T.void [i32, i16] False)) (UnName 0))),
               arguments' = [
                (ConstantOperand (C.Int 32 4), []),
                (ConstantOperand (C.Int 16 8), [])
               ],
               functionAttributes' = [],
               returnDest = Name "foo",
               exceptionDest = Name "bar",
               metadata' = []
              }
             ),
            BasicBlock (Name "foo") [] (
              Do $ Ret Nothing []
             ),
            BasicBlock (Name "bar") [
             UnName 3 := LandingPad {
               type' = StructureType False [
                  ptr i8,
                  i32
                 ],
               cleanup = True,
               clauses = [Catch (C.Null (ptr i8))],
               metadata = []
             }
             ] (
              Do $ Ret Nothing []
             )
           ]
         }
        ],
       "; ModuleID = '<string>'\n\
       \source_filename = \"<string>\"\n\
       \\n\
       \define void @0(i32, i16) personality void (i32, i16)* @0 {\n\
       \  invoke void @0(i32 4, i16 8)\n\
       \          to label %foo unwind label %bar\n\
       \\n\
       \foo:                                              ; preds = %2\n\
       \  ret void\n\
       \\n\
       \bar:                                              ; preds = %2\n\
       \  %3 = landingpad { i8*, i32 }\n\
       \          cleanup\n\
       \          catch i8* null\n\
       \  ret void\n\
       \}\n"
      ), (
       "resume",
       Module "<string>" "<string>" Nothing Nothing [
         GlobalDefinition $ functionDefaults {
           G.returnType = A.T.void,
           G.name = UnName 0,
           G.basicBlocks = [
             BasicBlock (UnName 0) [] (
               Do $ Resume (ConstantOperand (C.Int 32 1)) []
              )
            ]
          }
        ],
       "; ModuleID = '<string>'\n\
       \source_filename = \"<string>\"\n\
       \\n\
       \define void @0() {\n\
       \  resume i32 1\n\
       \}\n"
     ), (
       "unreachable",
       Module "<string>" "<string>" Nothing Nothing [
        GlobalDefinition $ functionDefaults {
          G.returnType = A.T.void,
          G.name = UnName 0,
          G.basicBlocks = [
            BasicBlock (UnName 0) [] (
              Do $ Unreachable []
             )
           ]
         }
        ],
       "; ModuleID = '<string>'\n\
       \source_filename = \"<string>\"\n\
       \\n\
       \define void @0() {\n\
       \  unreachable\n\
       \}\n"
     ), ( -- This testcase is taken from test/Feature/exception.ll in LLVM
       "cleanupret0",
       Module {
         moduleName = "<string>",
         moduleSourceFileName = "<string>",
         moduleDataLayout = Nothing,
         moduleTargetTriple = Nothing,
         moduleDefinitions = [
           GlobalDefinition functionDefaults {
             G.returnType = VoidType,
             G.name = Name "_Z3quxv"
           },
           GlobalDefinition functionDefaults {
             G.returnType = IntegerType {typeBits = 32},
             G.name = Name "__gxx_personality_v0",
             G.parameters = ([], True)
           },
           GlobalDefinition functionDefaults {
             G.returnType = VoidType,
             G.name = Name "cleanupret0",
             G.basicBlocks = [
               G.BasicBlock (Name "entry") [] (
                 Do Invoke {
                   callingConvention' = CC.C,
                   returnAttributes' = [],
                   function' = Right (
                     ConstantOperand (
                       C.GlobalReference PointerType {
                         pointerReferent = FunctionType {resultType = VoidType, argumentTypes = [], isVarArg = False},
                         pointerAddrSpace = AddrSpace 0
                       } (Name "_Z3quxv")
                     )
                   ),
                   arguments' = [],
                   returnDest = Name "exit",
                   exceptionDest = Name "pad",
                   metadata' = [],
                   functionAttributes' = []
                 }
               ),
               G.BasicBlock
                 (Name "pad")
                 [Name "cp" := CleanupPad { parentPad = ConstantOperand C.TokenNone, args = [ConstantOperand C.Int { C.integerBits = 7, C.integerValue = 4 }], metadata = [] }]
                 (
                 Do CleanupRet {
                   cleanupPad = LocalReference TokenType (Name "cp"),
                   unwindDest = Nothing,
                   metadata' = []
                 }
                 ),
               G.BasicBlock (Name "exit") [] (Do Ret { returnOperand = Nothing, metadata' = [] })
             ],
             G.personalityFunction = Just (
               C.GlobalReference PointerType {
                 pointerReferent = FunctionType {resultType = IntegerType { typeBits = 32 }, argumentTypes = [], isVarArg = True},
                 pointerAddrSpace = AddrSpace 0
               } (Name "__gxx_personality_v0")
             )
           }
         ]
       },
       "; ModuleID = '<string>'\n\
       \source_filename = \"<string>\"\n\
       \\n\
       \declare void @_Z3quxv()\n\
       \\n\
       \declare i32 @__gxx_personality_v0(...)\n\
       \\n\
       \define void @cleanupret0() personality i32 (...)* @__gxx_personality_v0 {\n\
       \entry:\n\
       \  invoke void @_Z3quxv()\n\
       \          to label %exit unwind label %pad\n\
       \\n\
       \pad:                                              ; preds = %entry\n\
       \  %cp = cleanuppad within none [i7 4]\n\
       \  cleanupret from %cp unwind to caller\n\
       \\n\
       \exit:                                             ; preds = %entry\n\
       \  ret void\n\
       \}\n"
     ), ( -- This testcase is taken from test/Feature/exception.ll in LLVM
       "cleanupret1",
       Module {
         moduleName = "<string>",
         moduleSourceFileName = "<string>",
         moduleDataLayout = Nothing,
         moduleTargetTriple = Nothing,
         moduleDefinitions = [
           GlobalDefinition functionDefaults {
             G.returnType = VoidType,
             G.name = Name "_Z3quxv"
           },
           GlobalDefinition functionDefaults {
             G.returnType = IntegerType {typeBits = 32},
             G.name = Name "__gxx_personality_v0",
             G.parameters = ([], True)
           },
           GlobalDefinition functionDefaults {
             G.returnType = VoidType,
             G.name = Name "cleanupret1",
             G.basicBlocks = [
               G.BasicBlock (Name "entry") [] (
                 Do Invoke {
                   callingConvention' = CC.C,
                   returnAttributes' = [],
                   function' = Right (
                     ConstantOperand (
                       C.GlobalReference PointerType {
                         pointerReferent = FunctionType {resultType = VoidType, argumentTypes = [], isVarArg = False},
                         pointerAddrSpace = AddrSpace 0
                       } (Name "_Z3quxv")
                     )
                   ),
                   arguments' = [],
                   returnDest = Name "exit",
                   exceptionDest = Name "pad",
                   metadata' = [],
                   functionAttributes' = []
                 }
               ),
               G.BasicBlock
                 (Name "cleanup")
                 []
                 (Do CleanupRet {
                    cleanupPad = LocalReference TokenType (Name "cp"),
                    unwindDest = Nothing,
                    metadata' = []
                 }),
               G.BasicBlock
                 (Name "pad")
                 [Name "cp" := CleanupPad { parentPad = ConstantOperand C.TokenNone, args = [], metadata = [] }]
                 (Do Br { dest = Name "cleanup", metadata' = [] }),
               G.BasicBlock (Name "exit") [] (Do Ret { returnOperand = Nothing, metadata' = [] })
             ],
             G.personalityFunction = Just (
               C.GlobalReference PointerType {
                 pointerReferent = FunctionType {resultType = IntegerType { typeBits = 32 }, argumentTypes = [], isVarArg = True},
                 pointerAddrSpace = AddrSpace 0
               } (Name "__gxx_personality_v0")
             )
           }
         ]
       },
       "; ModuleID = '<string>'\n\
       \source_filename = \"<string>\"\n\
       \\n\
       \declare void @_Z3quxv()\n\
       \\n\
       \declare i32 @__gxx_personality_v0(...)\n\
       \\n\
       \define void @cleanupret1() personality i32 (...)* @__gxx_personality_v0 {\n\
       \entry:\n\
       \  invoke void @_Z3quxv()\n\
       \          to label %exit unwind label %pad\n\
       \\n\
       \cleanup:                                          ; preds = %pad\n\
       \  cleanupret from %cp unwind to caller\n\
       \\n\
       \pad:                                              ; preds = %entry\n\
       \  %cp = cleanuppad within none []\n\
       \  br label %cleanup\n\
       \\n\
       \exit:                                             ; preds = %entry\n\
       \  ret void\n\
       \}\n"
     ), ( -- This testcase is taken from test/Feature/exception.ll in LLVM
       "catchret0",
       Module {
         moduleName = "<string>",
         moduleSourceFileName = "<string>",
         moduleDataLayout = Nothing,
         moduleTargetTriple = Nothing,
         moduleDefinitions = [
           GlobalDefinition functionDefaults {
             G.returnType = VoidType,
             G.name = Name "_Z3quxv"
           },
           GlobalDefinition functionDefaults {
             G.returnType = IntegerType {typeBits = 32},
             G.name = Name "__gxx_personality_v0",
             G.parameters = ([], True)
           },
           GlobalDefinition functionDefaults {
             G.returnType = VoidType,
             G.name = Name "catchret0",
             G.basicBlocks = [
               G.BasicBlock (Name "entry") [] (
                 Do Invoke {
                   callingConvention' = CC.C,
                   returnAttributes' = [],
                   function' = Right (
                     ConstantOperand (
                       C.GlobalReference PointerType {
                         pointerReferent = FunctionType {resultType = VoidType, argumentTypes = [], isVarArg = False},
                         pointerAddrSpace = AddrSpace 0
                       } (Name "_Z3quxv")
                     )
                   ),
                   arguments' = [],
                   returnDest = Name "exit",
                   exceptionDest = Name "pad",
                   metadata' = [],
                   functionAttributes' = []
                 }
               ),
               G.BasicBlock (Name "pad") [] (
                 Name "cs1" := CatchSwitch {
                   parentPad' = ConstantOperand C.TokenNone,
                   catchHandlers = (Name "catch" :| []),
                   defaultUnwindDest = Nothing,
                   metadata' = []
                 }
               ),
               G.BasicBlock
                 (Name "catch")
                 [Name "cp" := CatchPad { catchSwitch = LocalReference TokenType (Name "cs1"), args = [ConstantOperand C.Int { C.integerBits = 7, C.integerValue = 4 }], metadata = [] }] (
                 Do CatchRet {
                   catchPad = LocalReference TokenType (Name "cp"),
                   successor = Name "exit",
                   metadata' = []
                 }
               ),
               G.BasicBlock (Name "exit") [] (Do Ret { returnOperand = Nothing, metadata' = [] })
             ],
             G.personalityFunction = Just (
               C.GlobalReference PointerType {
                 pointerReferent = FunctionType {resultType = IntegerType { typeBits = 32 }, argumentTypes = [], isVarArg = True},
                 pointerAddrSpace = AddrSpace 0
               } (Name "__gxx_personality_v0")
             )
           }
         ]
       },
       "; ModuleID = '<string>'\n\
       \source_filename = \"<string>\"\n\
       \\n\
       \declare void @_Z3quxv()\n\
       \\n\
       \declare i32 @__gxx_personality_v0(...)\n\
       \\n\
       \define void @catchret0() personality i32 (...)* @__gxx_personality_v0 {\n\
       \entry:\n\
       \  invoke void @_Z3quxv()\n\
       \          to label %exit unwind label %pad\n\
       \\n\
       \pad:                                              ; preds = %entry\n\
       \  %cs1 = catchswitch within none [label %catch] unwind to caller\n\
       \\n\
       \catch:                                            ; preds = %pad\n\
       \  %cp = catchpad within %cs1 [i7 4]\n\
       \  catchret from %cp to label %exit\n\
       \\n\
       \exit:                                             ; preds = %catch, %entry\n\
       \  ret void\n\
       \}\n"
     ), ( -- This testcase is taken from test/Feature/exception.ll in LLVM
       "catchret1",
       Module {
         moduleName = "<string>",
         moduleSourceFileName = "<string>",
         moduleDataLayout = Nothing,
         moduleTargetTriple = Nothing,
         moduleDefinitions = [
           GlobalDefinition functionDefaults {
             G.returnType = VoidType,
             G.name = Name "_Z3quxv"
           },
           GlobalDefinition functionDefaults {
             G.returnType = IntegerType {typeBits = 32},
             G.name = Name "__gxx_personality_v0",
             G.parameters = ([], True)
           },
           GlobalDefinition functionDefaults {
             G.returnType = VoidType,
             G.name = Name "catchret0",
             G.basicBlocks = [
               G.BasicBlock (Name "entry") [] (
                 Do Invoke {
                   callingConvention' = CC.C,
                   returnAttributes' = [],
                   function' = Right (
                     ConstantOperand (
                       C.GlobalReference PointerType {
                         pointerReferent = FunctionType {resultType = VoidType, argumentTypes = [], isVarArg = False},
                         pointerAddrSpace = AddrSpace 0
                       } (Name "_Z3quxv")
                     )
                   ),
                   arguments' = [],
                   returnDest = Name "exit",
                   exceptionDest = Name "pad",
                   metadata' = [],
                   functionAttributes' = []
                 }
               ),
               G.BasicBlock (Name "catchret") [] (
                 Do CatchRet {
                    catchPad = LocalReference TokenType (Name "cp"),
                    successor = Name "exit",
                    metadata' = []
                 }
               ),
               G.BasicBlock (Name "pad") [] (
                 Name "cs1" := CatchSwitch {
                   parentPad' = ConstantOperand C.TokenNone,
                   catchHandlers = (Name "catch" :| []),
                   defaultUnwindDest = Nothing,
                   metadata' = []
                 }
               ),
               G.BasicBlock
                 (Name "catch")
                 [Name "cp" := CatchPad { catchSwitch = LocalReference TokenType (Name "cs1"), args = [ConstantOperand C.Int { C.integerBits = 7, C.integerValue = 4 }], metadata = [] }] (
                 Do Br { dest = (Name "catchret"), metadata' = [] }
               ),
               G.BasicBlock (Name "exit") [] (Do Ret { returnOperand = Nothing, metadata' = [] })
             ],
             G.personalityFunction = Just (
               C.GlobalReference PointerType {
                 pointerReferent = FunctionType {resultType = IntegerType { typeBits = 32 }, argumentTypes = [], isVarArg = True},
                 pointerAddrSpace = AddrSpace 0
               } (Name "__gxx_personality_v0")
             )
           }
         ]
       },
       "; ModuleID = '<string>'\n\
       \source_filename = \"<string>\"\n\
       \\n\
       \declare void @_Z3quxv()\n\
       \\n\
       \declare i32 @__gxx_personality_v0(...)\n\
       \\n\
       \define void @catchret0() personality i32 (...)* @__gxx_personality_v0 {\n\
       \entry:\n\
       \  invoke void @_Z3quxv()\n\
       \          to label %exit unwind label %pad\n\
       \\n\
       \catchret:                                         ; preds = %catch\n\
       \  catchret from %cp to label %exit\n\
       \\n\
       \pad:                                              ; preds = %entry\n\
       \  %cs1 = catchswitch within none [label %catch] unwind to caller\n\
       \\n\
       \catch:                                            ; preds = %pad\n\
       \  %cp = catchpad within %cs1 [i7 4]\n\
       \  br label %catchret\n\
       \\n\
       \exit:                                             ; preds = %catchret, %entry\n\
       \  ret void\n\
       \}\n"
     )
    ]
   ],

  testGroup "fast-math flags" [
    testProperty "roundtrip" $ \flags ->
     ioProperty $ withContext $ \ctx -> do
       encodedFlags <- encodeM flags :: IO FFI.FastMathFlags
       decodedFlags <- decodeM encodedFlags :: IO FastMathFlags
       pure (decodedFlags === flags)
  ]
 ]

instance Arbitrary FastMathFlags where
  arbitrary = oneof
    [ pure noFastMathFlags
    , FastMathFlags <$>
       arbitrary <*>
       arbitrary <*>
       arbitrary <*>
       arbitrary <*>
       arbitrary <*>
       arbitrary <*>
       arbitrary
    ]
