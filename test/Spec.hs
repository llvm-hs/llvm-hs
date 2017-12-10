{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
module Main
  ( main
  ) where

import           Data.Monoid
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T
import           LLVM.AST hiding (function)
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.Float as F
import           LLVM.AST.Type as AST
import           LLVM.Pretty (ppllvm)
import           Test.Hspec hiding (example)

import           LLVM.IRBuilder

main :: IO ()
main =
  hspec $ do
    describe "module builder" $ do
      it "builds the simple module" $
        ppllvm simple <> "\n" `shouldBe`
        T.unlines
          [ "; ModuleID = 'exampleModule'"
          , ""
          , "define external ccc i32 @add(i32 %a, i32 %b){"
          , "entry:"
          , "  %0 = add i32 %a, %b"
          , "  ret i32 %0"
          , "}"
          ]
      it "builds the example" $
        ppllvm example <> "\n" `shouldBe`
        T.unlines
          [ "; ModuleID = 'exampleModule'"
          , ""
          , "define external ccc double @foo(){"
          , "; <label>:0:"
          , "  %xxx = fadd double 1.000000e1, 1.000000e1"
          , "  ret void"
          , "blk:"
          , "  %1 = fadd double 1.000000e1, 1.000000e1"
          , "  %2 = fadd double %1, %1"
          , "  %3 = add i32 10, 10"
          , "  br label %blk1"
          , "blk1:"
          , "  %c = fadd double 1.000000e1, 1.000000e1"
          , "  %4 = fadd double %c, %c"
          , "  br label %blk2"
          , "blk2:"
          , "  %phi = phi double [1.000000e1, %blk], [1.000000e1, %blk1], [1.000000e1, %blk2]"
          , "  %5 = fadd double 1.000000e1, 1.000000e1"
          , "  %6 = fadd double %5, %5"
          , "  ret void"
          , "}"
          , ""
          , "define external ccc double @bar(){"
          , "  %1 = fadd double 1.000000e1, 1.000000e1"
          , "  %2 = fadd double %1, %1"
          , "  ret void"
          , "}"
          , ""
          , "define external ccc double @baz(i32, double %arg, i32, double %arg1){"
          , "; <label>:2:"
          , "  switch i32 %1, label %3 [i32 0, label %4 i32 1, label %7]"
          , "; <label>:3:"
          , "  br label %4"
          , "; <label>:4:"
          , "  %arg2 = fadd double %arg, 1.000000e1"
          , "  %5 = fadd double %arg2, %arg2"
          , "  %6 = select i1 0, double %arg2, double %5"
          , "  ret void"
          , "; <label>:7:"
          , "  %8 = getelementptr i32**, i32*** zeroinitializer, i32 10, i32 20, i32 30"
          , "  %9 = getelementptr i32, i32* %8, i32 40"
          , "  ret void"
          , "}"
          ]

simple :: Module
simple = buildModule "exampleModule" $ mdo

  function "add" [(i32, "a"), (i32, "b")] i32 $ \[a, b] -> mdo

    entry <- block `named` "entry"; do
      c <- add a b
      ret c

example :: Module
example = mkModule $ execModuleBuilder emptyModuleBuilder $ mdo

  foo <- function "foo" [] double $ \_ -> mdo
    xxx <- fadd c1 c1 `named` "xxx"

    blk1 <- block `named` "blk"; do
      a <- fadd c1 c1
      b <- fadd a a
      c <- add c2 c2
      br blk2

    blk2 <- block `named` "blk"; do
      a <- fadd c1 c1 `named` "c"
      b <- fadd a a
      br blk3

    blk3 <- block `named` "blk"; do
      l <- phi [(c1, blk1), (c1, blk2), (c1, blk3)] `named` "phi"
      a <- fadd c1 c1
      b <- fadd a a
      retVoid

    pure ()


  function "bar" [] double $ \_ -> mdo

    blk3 <- block; do
      a <- fadd c1 c1
      b <- fadd a a
      retVoid

    pure ()

  function "baz" [(i32, NoParameterName), (double, "arg"), (i32, NoParameterName), (double, "arg")] double $ \[rrr, arg, arg2, arg3] -> mdo

    switch arg2 blk1 [(C.Int 32 0, blk2), (C.Int 32 1, blk3)]

    blk1 <- block; do
      br blk2

    blk2 <- block; do
      a <- fadd arg c1 `named` "arg"
      b <- fadd a a
      select (cons $ C.Int 1 0) a b
      retVoid

    blk3 <- block; do
      let nul = cons $ C.Null $ ptr $ ptr $ ptr $ IntegerType 32
      addr <- gep nul [cons $ C.Int 32 10, cons $ C.Int 32 20, cons $ C.Int 32 30]
      addr' <- gep addr [cons $ C.Int 32 40]
      retVoid

    pure ()
  where
    mkModule ds = defaultModule { moduleName = "exampleModule", moduleDefinitions = ds }
    cons = ConstantOperand

c1 :: Operand
c1 = ConstantOperand $ C.Float (F.Double 10)

c2 :: Operand
c2 = ConstantOperand $ C.Int 32 10
