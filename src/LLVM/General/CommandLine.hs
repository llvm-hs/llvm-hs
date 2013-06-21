-- | Tools for processing command line arguments, for command line tools build
-- with llvm (or for other uses forced into pretending to be such to get at (ack)
-- global state).
module LLVM.General.CommandLine (
  parseCommandLineOptions
) where

import LLVM.General.Internal.CommandLine