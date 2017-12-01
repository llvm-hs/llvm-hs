{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module IRBuilder
  (module X, example)
  where

import Data.Text.Lazy.IO as T

import LLVM.Pretty
import LLVM.AST hiding (function)
import LLVM.AST.Type as AST
import qualified LLVM.AST.Float as F
import qualified LLVM.AST.Constant as C

import IRBuilder.Monad as X
import IRBuilder.Instruction as X
import ModuleBuilder as X

-------------------------------------------------------------------------------
-- Testing
-------------------------------------------------------------------------------

c1 :: Operand
c1 = ConstantOperand $ C.Float (F.Double 10)

c2 :: Operand
c2 = ConstantOperand $ C.Int 32 10

simple :: IO ()
simple = T.putStrLn $ ppllvm $ buildModule "exampleModule" $ mdo

  function "add" [(i32, "a"), (i32, "b")] i32 $ \[a, b] -> mdo

    entry <- block `named` "entry"; do
      c <- add a b
      ret c

example :: IO ()
example = T.putStrLn $ ppllvm $ mkModule $ execModuleBuilder emptyModuleBuilder $ mdo

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
