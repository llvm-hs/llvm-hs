-- | Tools for processing command line arguments, for command line tools build
-- with llvm (or for other uses forced into pretending to be such to get at (ack)
-- global state).
module LLVM.CommandLine (
  parseCommandLineOptions
) where

import LLVM.Internal.CommandLine