module LLVM.IRBuilder.Constant where
import           Data.Word
import           LLVM.Prelude
import           LLVM.AST             hiding (args, dests)
import           LLVM.AST.Typed

import           LLVM.AST.Constant
import           LLVM.AST.Float

int64 :: Applicative f => Integer -> f Operand
int64 = pure . ConstantOperand . Int 64

int32 :: Applicative f => Integer -> f Operand
int32 = pure . ConstantOperand . Int 32

bit :: Applicative f => Integer -> f Operand
bit = pure . ConstantOperand . Int 1

double :: Applicative f => Double -> f Operand
double = pure . ConstantOperand . Float . Double

single :: Applicative f => Float -> f Operand
single = pure . ConstantOperand . Float . Single

half :: Applicative f => Word16 -> f Operand
half = pure . ConstantOperand . Float . Half

struct :: Applicative f => Maybe Name -> Bool -> [Constant] -> f Operand
struct nm packing members = pure . ConstantOperand $ Struct nm packing members

array :: Applicative f => [Constant] -> f Operand
array members = pure . ConstantOperand $ Array (typeOf $ head members) members
