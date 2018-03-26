{-|
This module lists all of the exceptions thrown by 'llvm-hs' itself.
Note that other exceptions can potentially be thrown
by the underlying libraries, e.g., for functions doing file IO.
-}
module LLVM.Exception where

import LLVM.Prelude

import Control.Monad.Catch

-- | Indicates an error during the translation of the AST provided by
-- 'llvm-hs-pure' to LLVM’s internal representation.
data EncodeException =
  EncodeException !String
  deriving (Show, Eq, Ord, Typeable)

instance Exception EncodeException

-- | Indicates an error during the translation of LLVM’s internal representation
-- to the AST provided 'llvm-hs-pure'.
data DecodeException =
  DecodeException !String
  deriving (Show, Eq, Ord, Typeable)

instance Exception DecodeException

-- | Indicates an error during the parsing of a module. This is used
-- for errors encountered when parsing LLVM’s human readable assembly
-- format and when parsing the binary bitcode format.
data ParseFailureException =
  ParseFailureException !String
  deriving (Show, Eq, Ord, Typeable)

instance Exception ParseFailureException

-- | Indicates an error during the linking of two modules.
data LinkException =
  LinkException !String
  deriving (Show, Eq, Ord, Typeable)

instance Exception LinkException

-- | Indicates an error during the creation of a
-- <http://llvm.org/docs/doxygen/html/classllvm_1_1raw__fd__ostream.html raw_fd_ostream>.
-- This could be caused by a nonexisting file path.
data FdStreamException =
  FdStreamException !String
  deriving (Show, Eq, Ord, Typeable)

instance Exception FdStreamException

-- | Indicates an error during a call to 'LLVM.Internal.Module.targetMachineEmit'.
data TargetMachineEmitException =
  TargetMachineEmitException !String
  deriving (Show, Eq, Ord, Typeable)

instance Exception TargetMachineEmitException

-- | Indicates a failure to find the target.
data LookupTargetException =
  LookupTargetException !String
  deriving (Show, Eq, Ord, Typeable)

instance Exception LookupTargetException

-- | Indicates an error during the verification of a module.
data VerifyException =
  VerifyException !String
  deriving (Show, Eq, Ord, Typeable)

instance Exception VerifyException
