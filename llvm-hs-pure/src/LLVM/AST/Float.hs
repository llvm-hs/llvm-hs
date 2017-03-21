-- | This module provides a sub-namespace for a type to support the various sizes of floating point
-- numbers LLVM supports. It is most definitely intended to be imported qualified.
module LLVM.AST.Float where

import LLVM.Prelude

-- | A type summing up the various float types.
-- N.B. Note that in the constructors with multiple fields, the lower significance bits are on the right
-- - e.g. Quadruple highbits lowbits
data SomeFloat
  = Half Word16
  | Single Float
  | Double Double
  | Quadruple Word64 Word64
  | X86_FP80 Word16 Word64
  | PPC_FP128 Word64 Word64
  deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)

