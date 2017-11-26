IRBuilder
=========

[![Build Status](https://travis-ci.org/llvm-hs/llvm-irbuilder.svg?branch=master)](https://travis-ci.org/llvm-hs/llvm-irbuilder)

A IRBuilder, starting out as a thin reinterpretation of the C++ IRBuilder inside
of a Haskell State monad. Goal is to eliminate a lot of boilerplate around the
most common uses of `llvm-hs` as a compiler backend. 

Example LLVM module that adds two numbers:

```llvm
; ModuleID = 'exampleModule'

define external ccc i32 @add(i32 %a, i32 %b){
entry:
  %0 = add i32 %a, %b
  ret i32 %0
}
```

```haskell
import Data.Text.Lazy.IO as T

import LLVM.Pretty
import LLVM.AST hiding (function)
import LLVM.AST.Type as AST
import qualified LLVM.AST.Float as F
import qualified LLVM.AST.Constant as C

import ModuleBuilder
import IRBuilder.Monad
import IRBuilder.Instruction

simple :: IO ()
simple = T.putStrLn $ ppllvm $ mkModule $ execModuleBuilder emptyModuleBuilder $ mdo

  function "add" [(i32, Just "a"), (i32, (Just "b"))] i32 $ \[a,b] -> mdo

    entry <- block `named` "entry"; do
      c <- add a b
      ret c

  where
    mkModule ds = defaultModule { moduleName = "exampleModule", moduleDefinitions = ds }
```

License
-------

MIT
