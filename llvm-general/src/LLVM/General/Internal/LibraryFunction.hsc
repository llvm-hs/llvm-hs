{-# LANGUAGE
  MultiParamTypeClasses
 #-}
module LLVM.General.Internal.LibraryFunction where

import qualified LLVM.General.Internal.FFI.LLVMCTypes as FFI

import LLVM.General.Internal.Coding

#include "LLVM/General/Internal/FFI/LibFunc.h"

#{
define hsc_inject(m) { \
  struct { const char *s; unsigned n; } *p, list[] = { LLVM_GENERAL_FOR_EACH_LIB_FUNC(m) }; \
  hsc_printf("data LibraryFunction\n"); \
  for(p = list; p < list + sizeof(list)/sizeof(list[0]); ++p) { \
    hsc_printf("  %s LibraryFunction__%s\n", (p == list ? "=" : "|"), p->s); \
  } \
  hsc_printf("\n"); \
  hsc_printf("instance Monad m => EncodeM m LibraryFunction FFI.LibFunc where\n"); \
  for(p = list; p < list + sizeof(list)/sizeof(list[0]); ++p) { \
    hsc_printf("  encodeM LibraryFunction__%s = return (FFI.LibFunc %u)\n", p->s, p->n); \
  } \
  hsc_printf("\n"); \
  hsc_printf("instance Monad m => DecodeM m LibraryFunction FFI.LibFunc where\n"); \
  for(p = list; p < list + sizeof(list)/sizeof(list[0]); ++p) { \
    hsc_printf("  decodeM (FFI.LibFunc %u) = return LibraryFunction__%s \n", p->n, p->s); \
  } \
}
}

#define Mac(n) { #n, LLVMLibFunc__ ## n },
#{inject Mac}
