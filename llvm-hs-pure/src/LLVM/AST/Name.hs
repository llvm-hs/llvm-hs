-- | Names as used in LLVM IR
module LLVM.AST.Name where

import LLVM.Prelude
import Data.Char
import Data.String

{- |
Objects of various sorts in LLVM IR are identified by address in the LLVM C++ API, and
may be given a string name. When printed to (resp. read from) human-readable LLVM assembly, objects without
string names are numbered sequentially (resp. must be numbered sequentially). String names may be quoted, and
are quoted when printed if they would otherwise be misread - e.g. when containing special characters.

> 7

means the seventh unnamed object, while

> "7"

means the object named with the string "7".

This libraries handling of 'UnName's during translation of the AST down into C++ IR is somewhat more
forgiving than the LLVM assembly parser: it does not require that unnamed values be numbered sequentially;
however, the numbers of 'UnName's passed into C++ cannot be preserved in the C++ objects. If the C++ IR is
printed as assembly or translated into a Haskell AST, unnamed nodes will be renumbered sequentially. Thus
unnamed node numbers should be thought of as having any scope limited to the 'LLVM.AST.Module' in
which they are used.
-}
data Name
    = Name ShortByteString -- ^ a string name
    | UnName Word -- ^ a number for a nameless thing
   deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)

-- | Using 'fromString` on non-ASCII strings will throw an error.
instance IsString Name where
  fromString s
    | all isAscii s = Name (fromString s)
    | otherwise =
      error ("Only ASCII strings are automatically converted to LLVM names. "
          <> "Other strings need to be encoded to a `ShortByteString` using an arbitrary encoding.")

-- | Create a 'Name' based on an ASCII 'String'.
-- Non-ASCII strings will throw an error.
mkName :: String -> Name
mkName = fromString
