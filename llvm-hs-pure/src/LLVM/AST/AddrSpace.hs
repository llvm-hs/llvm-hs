-- | Pointers exist in Address Spaces 
module LLVM.AST.AddrSpace where

import LLVM.Prelude

-- | See <http://llvm.org/docs/LangRef.html#pointer-type>
data AddrSpace = AddrSpace Word32
   deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)
