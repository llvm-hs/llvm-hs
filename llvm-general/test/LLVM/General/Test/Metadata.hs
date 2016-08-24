module LLVM.General.Test.Metadata where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit

import LLVM.General.Test.Support

import LLVM.General.AST as A
import LLVM.General.AST.Type as A.T
import LLVM.General.AST.AddrSpace as A
import qualified LLVM.General.AST.Linkage as L
import qualified LLVM.General.AST.Visibility as V
import qualified LLVM.General.AST.CallingConvention as CC
import qualified LLVM.General.AST.Constant as C
import LLVM.General.AST.Global as G

tests = testGroup "Metadata" [
  -- testCase "local" $ do
  --   let ast = Module "<string>" Nothing Nothing [
  --         GlobalDefinition $ globalVariableDefaults { G.name = UnName 0, G.type' = i32 },
  --         GlobalDefinition $ functionDefaults {
  --           G.returnType = i32,
  --           G.name = Name "foo",
  --           G.basicBlocks = [
  --             BasicBlock (UnName 0) [
  --                UnName 1 := Load {
  --                           volatile = False,
  --                           address = ConstantOperand (C.GlobalReference (ptr i32) (UnName 0)),
  --                           maybeAtomicity = Nothing,
  --                           A.alignment = 0,
  --                           metadata = []
  --                         }
  --                ] (
  --                Do $ Ret (Just (ConstantOperand (C.Int 32 0))) [
  --                  (
  --                    "my-metadatum", 
  --                    MetadataNode [
  --                     Just $ MDValue $ LocalReference i32 (UnName 1),
  --                     Just $ MDString "super hyper",
  --                     Nothing
  --                    ]
  --                  )
  --                ]
  --              )
  --            ]
  --          }
  --        ]
  --   let s = "; ModuleID = '<string>'\n\
  --           \\n\
  --           \@0 = external global i32\n\
  --           \\n\
  --           \define i32 @foo() {\n\
  --           \  %1 = load i32, i32* @0\n\
  --           \  ret i32 0, !my-metadatum !{i32 %1, !\"super hyper\", null}\n\
  --           \}\n"
  --   strCheck ast s,

  testCase "global" $ do
    let ast = Module "<string>" "<string>" Nothing Nothing [
          GlobalDefinition $ functionDefaults {
            G.returnType = i32,
            G.name = Name "foo",
            G.basicBlocks = [
              BasicBlock (UnName 0) [
              ] (
                Do $ Ret (Just (ConstantOperand (C.Int 32 0))) [
                  ("my-metadatum", MetadataNodeReference (MetadataNodeID 0))
                ]
              )
             ]
            },
          MetadataNodeDefinition (MetadataNodeID 0) [ Just $ MDValue $ ConstantOperand (C.Int 32 1) ]
         ]
    let s = "; ModuleID = '<string>'\n\
            \source_filename = \"<string>\"\n\
            \\n\
            \define i32 @foo() {\n\
            \  ret i32 0, !my-metadatum !0\n\
            \}\n\
            \\n\
            \!0 = !{i32 1}\n"
    strCheck ast s,

  testCase "named" $ do
    let ast = Module "<string>" "<string>" Nothing Nothing [
          NamedMetadataDefinition "my-module-metadata" [ MetadataNodeID 0 ],
          MetadataNodeDefinition (MetadataNodeID 0) [ Just $ MDValue $ ConstantOperand (C.Int 32 1) ]
         ]
    let s = "; ModuleID = '<string>'\n\
            \source_filename = \"<string>\"\n\
            \\n\
            \!my-module-metadata = !{!0}\n\
            \\n\
            \!0 = !{i32 1}\n"
    strCheck ast s,

  testCase "null" $ do
    let ast = Module "<string>" "<string>" Nothing Nothing [
          NamedMetadataDefinition "my-module-metadata" [ MetadataNodeID 0 ],
          MetadataNodeDefinition (MetadataNodeID 0) [ Nothing ]
         ]
    let s = "; ModuleID = '<string>'\n\
            \source_filename = \"<string>\"\n\
            \\n\
            \!my-module-metadata = !{!0}\n\
            \\n\
            \!0 = !{null}\n"
    strCheck ast s,

  testGroup "cyclic" [
    testCase "metadata-only" $ do
      let ast = Module "<string>" "<string>" Nothing Nothing [
            NamedMetadataDefinition "my-module-metadata" [MetadataNodeID 0],
            MetadataNodeDefinition (MetadataNodeID 0) [
              Just $ MDNode (MetadataNodeReference (MetadataNodeID 1)) 
             ],
            MetadataNodeDefinition (MetadataNodeID 1) [
              Just $ MDNode (MetadataNodeReference (MetadataNodeID 0)) 
             ]
           ]
      let s = "; ModuleID = '<string>'\n\
              \source_filename = \"<string>\"\n\
              \\n\
              \!my-module-metadata = !{!0}\n\
              \\n\
              \!0 = !{!1}\n\
              \!1 = !{!0}\n"
      strCheck ast s,

    testCase "metadata-global" $ do
      let ast = Module "<string>" "<string>" Nothing Nothing [
            GlobalDefinition $ functionDefaults {
              G.returnType = A.T.void,
              G.name = Name "foo",
              G.basicBlocks = [
                BasicBlock (UnName 0) [
                 ] (
                   Do $ Ret Nothing [ ("my-metadatum", MetadataNodeReference (MetadataNodeID 0)) ]
                 )
               ]
             },
            MetadataNodeDefinition (MetadataNodeID 0) [
              Just $ MDValue $ ConstantOperand (C.GlobalReference (ptr (FunctionType A.T.void [] False)) (Name "foo"))
             ]
           ]
      let s = "; ModuleID = '<string>'\n\
              \source_filename = \"<string>\"\n\
              \\n\
              \define void @foo() {\n\
              \  ret void, !my-metadatum !0\n\
              \}\n\
              \\n\
              \!0 = !{void ()* @foo}\n"
      strCheck ast s
   ]

 ]
