module LLVM.IRBuilder.Constant where
import           Data.Word
import           LLVM.Prelude
import           LLVM.AST             hiding (args, dests)
import           LLVM.AST.Typed

import           LLVM.AST.Constant
import           LLVM.AST.Float

int64 :: Integer -> Operand
int64 = ConstantOperand . Int 64
int32 :: Integer -> Operand
int32 = ConstantOperand . Int 32
int8 :: Integer -> Operand
int8  = ConstantOperand . Int 8
bit :: Integer -> Operand
bit   = ConstantOperand . Int 1

double :: Double -> Operand
double = ConstantOperand . Float . Double

single :: Float -> Operand
single = ConstantOperand . Float . Single

half :: Word16 -> Operand
half = ConstantOperand . Float . Half

struct :: Maybe Name -> Bool -> [Constant] -> Operand
struct nm packing members = ConstantOperand $ Struct nm packing members

array :: [Constant] -> Operand
array members = ConstantOperand $ Array (typeOf $ head members) members
