module LLVM.Test.ObjectCode where

import Test.Tasty
import Test.Tasty.HUnit

import LLVM.Test.Support

import qualified Data.ByteString as ByteString
import System.IO
import System.IO.Temp

import LLVM.Context
import LLVM.Module
import LLVM.Target

ll :: String
ll = unlines ["define i32 @main(i32 %argc, i8** %argv) {", "  ret i32 0", "}"]

tests =
  testGroup
    "Object code serialization"
    [ testCase
        "serialization to ByteString and to file" $ do
        withContext $ \ctx ->
          withSystemTempFile "foo" $ \objFile handle -> do
            hClose handle
            withHostTargetMachineDefault $ \machine ->
              withModuleFromLLVMAssembly ctx ll $ \mdl -> do
                obj <- moduleObject machine mdl
                _ <- writeObjectToFile machine (File objFile) mdl
                obj' <- ByteString.readFile objFile
                obj @=? obj'
    ]
