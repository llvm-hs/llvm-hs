-- | Names as used in LLVM IR
module LLVM.General.AST.Name where

import LLVM.General.Prelude

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
unnamed node numbers should be thought of as having any scope limited to the 'LLVM.General.AST.Module' in
which they are used.
-}
data Name 
    = Name String -- ^ a string name 
    | UnName Word -- ^ a number for a nameless thing
   deriving (Eq, Ord, Read, Show, Typeable, Data)

