-- This module translates the instruction data in "llvm/Instruction.def" into a Haskell data structure,
-- so it may be accessed conveniently with Template Haskell code
module LLVM.Internal.FFI.InstructionDefs where

import LLVM.Prelude

import LLVM.Internal.FFI.LLVMCTypes

#define FIRST_TERM_INST(num) struct inst { const char *kind; int opcode; const char *name; const char *clas; } insts[] = { { "Terminator", },

#define FIRST_BINARY_INST(num) { "Binary" },
#define FIRST_MEMORY_INST(num) { "Memory" },
#define FIRST_CAST_INST(num) { "Cast" },
#define FIRST_OTHER_INST(num) { "Other" },
#define FIRST_FUNCLETPAD_INST(num) { "FuncletPad" },

#define HANDLE_INST(num,opcode,class) { 0, num, #opcode, #class, },

#define LAST_OTHER_INST(num) { 0, 0, 0, 0, } };

#include "llvm/Config/llvm-config.h"

#include "llvm/IR/Instruction.def"

#{
define hsc_inject() {                                       \
  hsc_printf("[");                                       \
  struct inst *i;                                           \
  const char *kind = NULL;                                  \
  int first = 1;                                            \
  for(i = insts; i->kind || i->opcode; ++i) {               \
    if (i->kind) { kind = i->kind; continue; }              \
    if (!first) { hsc_printf("\n   ,"); } else { first = 0; }  \
    hsc_printf(                                             \
        " (CPPOpcode %d,\"%s\",\"%s\", %s)",               \
        i->opcode, i->name, i->clas, kind                   \
      );                                                    \
  }                                                         \
  hsc_printf(" ] ");                                       \
}
}

data InstructionKind = Terminator | Binary | Memory | Cast | FuncletPad | Other
  deriving (Eq, Ord, Show)

data InstructionDef = InstructionDef {
    cppOpcode :: CPPOpcode,
    cAPIName :: String,
    cAPIClassName :: String,
    instructionKind :: InstructionKind
  }
  deriving (Eq, Ord, Show)

instructionDefs :: [InstructionDef]
instructionDefs = [ 
 InstructionDef o an acn k
 | (o, an, acn, k) <-
   #{inject},
 an /= "UserOp1" && an /= "UserOp2"
 ]

