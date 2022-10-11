#include <iostream>
#include <memory>

#include <llvm/ExecutionEngine/Orc/CompileUtils.h>
#include <llvm/ExecutionEngine/Orc/Core.h>
#include <llvm/ExecutionEngine/Orc/ExecutionUtils.h>
#include <llvm/ExecutionEngine/Orc/Mangling.h>
#include <llvm/ExecutionEngine/Orc/IRCompileLayer.h>
#include <llvm/ExecutionEngine/Orc/RTDyldObjectLinkingLayer.h>
#include <llvm/ExecutionEngine/Orc/ObjectLinkingLayer.h>
#include <llvm/ExecutionEngine/JITLink/JITLinkMemoryManager.h>
#include <llvm/ExecutionEngine/SectionMemoryManager.h>
#include <llvm/Bitcode/BitcodeWriter.h>
#include <llvm/Bitcode/BitcodeReader.h>
#include <llvm/IR/Module.h>
#include <llvm-c/Core.h>
#include <llvm-c/Orc.h>

#include "LLVM/Internal/FFI/OrcJIT.h"
#include "LLVM/Internal/FFI/Target.hpp"

using namespace llvm;
using namespace orc;

#define SYMBOL_CASE(x)                                                         \
    static_assert((unsigned)LLVMJITSymbolFlag##x ==                            \
                      (unsigned)llvm::JITSymbolFlags::FlagNames::x,            \
                  "JITSymbolFlag values should agree");
LLVM_HS_FOR_EACH_JIT_SYMBOL_FLAG(SYMBOL_CASE)

static JITSymbolFlags unwrap(LLVMJITSymbolFlags_ f) {
    JITSymbolFlags flags = JITSymbolFlags::None;
#define ENUM_CASE(x)                                                           \
    if (f & LLVMJITSymbolFlag##x)                                              \
        flags |= JITSymbolFlags::x;
    LLVM_HS_FOR_EACH_JIT_SYMBOL_FLAG(ENUM_CASE)
#undef ENUM_CASE
    return flags;
}

static LLVMJITSymbolFlags_ wrap(JITSymbolFlags f) {
    unsigned r = 0;
#define ENUM_CASE(x)                                                           \
    if (f & JITSymbolFlags::x)                                                 \
        r |= (unsigned)LLVMJITSymbolFlag##x;
    LLVM_HS_FOR_EACH_JIT_SYMBOL_FLAG(ENUM_CASE)
#undef ENUM_CASE
    return LLVMJITSymbolFlags_(r);
}

extern "C" {

// ExecutionSession

ExecutionSession *LLVM_Hs_createExecutionSession() {
    return new ExecutionSession();
}

void LLVM_Hs_disposeExecutionSession(ExecutionSession *es) {
    delete es;
}

void LLVM_Hs_ExecutionSession_endSession(ExecutionSession *es) {
    if (Error err = es->endSession()) {
        llvm::errs() << err << "\n";
        // FIXME: Better error handling
        exit(1);
    }
}

// Thread-safe context

ThreadSafeContext* LLVM_Hs_createThreadSafeContext() {
    return new ThreadSafeContext(std::make_unique<LLVMContext>());
}

void LLVM_Hs_disposeThreadSafeContext(ThreadSafeContext* ctx) {
    delete ctx;
}

// Thread-safe module

// TODO: Figure out a way to do this without cloning the module
// The cloning is inspired by the code in llvm::orc::cloneToNewContext
ThreadSafeModule* LLVM_Hs_cloneAsThreadSafeModule(LLVMModuleRef m) {
    Module* module = unwrap(m);
    SmallVector<char,1> bitcode;
    BitcodeWriter writer(bitcode);
    writer.writeModule(*module);
    writer.writeSymtab();
    writer.writeStrtab();

    ThreadSafeContext clone_context(std::make_unique<LLVMContext>());
    MemoryBufferRef bitcode_ref(StringRef(bitcode.data(), bitcode.size()), "clone bitcode");
    std::unique_ptr<Module> cloned_module = cantFail(
        parseBitcodeFile(bitcode_ref, *clone_context.getContext()));
    cloned_module->setModuleIdentifier(module->getName());
    return new ThreadSafeModule(std::move(cloned_module), std::move(clone_context));
}

void LLVM_Hs_disposeThreadSafeModule(ThreadSafeModule* module) {
    delete module;
}

// Object layer

ObjectLayer* LLVM_Hs_createRTDyldObjectLinkingLayer(ExecutionSession* es) {
    return new RTDyldObjectLinkingLayer(*es, []() {
        return std::make_unique<SectionMemoryManager>();
    });
}

ObjectLayer* LLVM_Hs_createObjectLinkingLayer(ExecutionSession* es) {
    return new ObjectLinkingLayer(*es, std::make_unique<jitlink::InProcessMemoryManager>());
}

void LLVM_Hs_ObjectLayerAddObjectFile(ObjectLayer* ol, JITDylib* dylib, const char* path) {
  ExitOnError ExitOnErr;
  if (auto errorOrBuffer = MemoryBuffer::getFile(path)) {
    ExitOnErr(ol->add(*dylib, std::move(errorOrBuffer.get())));
  } else exit(1);
}

void LLVM_Hs_disposeObjectLayer(ObjectLayer* ol) {
    delete ol;
}

// Compile layer

IRLayer* LLVM_Hs_createIRCompileLayer(ExecutionSession* es, ObjectLayer* baseLayer, LLVMTargetMachineRef tm) {
    return new IRCompileLayer(*es, *baseLayer, std::make_unique<SimpleCompiler>(SimpleCompiler(*unwrap(tm))));
}

void LLVM_Hs_disposeIRLayer(IRLayer* il) {
    delete il;
}

void LLVM_Hs_JITDylib_addDynamicLibrarySearchGenerator_forCurrentProcess(JITDylib* dylib, LLVMTargetDataRef dataLayout) {
    auto dataLayoutCpp = *unwrap(dataLayout);
    ExitOnError ExitOnErr;
    dylib->addGenerator(
      ExitOnErr(orc::DynamicLibrarySearchGenerator::GetForCurrentProcess(
          dataLayoutCpp.getGlobalPrefix())));
}

void LLVM_Hs_JITDylib_addDynamicLibrarySearchGenerator_load(JITDylib* dylib, LLVMTargetDataRef dataLayout, const char* name) {
    auto dataLayoutCpp = *unwrap(dataLayout);
    ExitOnError ExitOnErr;
    dylib->addGenerator(
      ExitOnErr(orc::DynamicLibrarySearchGenerator::Load(
          name, dataLayoutCpp.getGlobalPrefix())));
}

void LLVM_Hs_JITDylib_defineAbsoluteSymbols(
    JITDylib* dylib, unsigned num_symbols, const SymbolStringPtr** names, const JITEvaluatedSymbol** symbols) {
  orc::SymbolMap map;
  for (unsigned i = 0; i < num_symbols; ++i) {
    map[*names[i]] = *symbols[i];
  }
  if (Error err = dylib->define(orc::absoluteSymbols(std::move(map)))) {
    llvm::errs() << err << "\n";
    // FIXME: Better error handling
    exit(1);
  }
}

// Warning: This consumes the module.
void LLVM_Hs_IRLayer_addModule(ThreadSafeModule* tsm, JITDylib* dylib, LLVMTargetDataRef dataLayout, IRLayer* il) {
    auto dataLayoutCpp = *unwrap(dataLayout);
    tsm->withModuleDo([&](auto& module) {
        if (module.getDataLayout().isDefault()) {
            module.setDataLayout(dataLayoutCpp);
        }
    });
    if (Error err = il->add(*dylib, std::move(*tsm))) {
        llvm::errs() << err << "\n";
        // FIXME: Better error handling
        exit(1);
    }
}

JITDylib* LLVM_Hs_ExecutionSession_createJITDylib(ExecutionSession* es, const char* name) {
    if (auto dylibOrErr = es->createJITDylib(name)) {
        auto& dylib = *dylibOrErr;
        return &dylib;
    } else {
        Error err = dylibOrErr.takeError();
        llvm::errs() << err << "\n";
        // FIXME: Better error handling
        exit(1);
    }
}


Expected<JITEvaluatedSymbol>* LLVM_Hs_ExecutionSession_lookupSymbol(
    ExecutionSession* es, JITDylib* dylib,
    MangleAndInterner* mangler, const char* name) {
    // Printing here will show unresolved symbols.
    // es->dump(llvm::errs());
    return new Expected<JITEvaluatedSymbol>(es->lookup({dylib}, (*mangler)(name)));
}

uint64_t LLVM_Hs_getExpectedJITEvaluatedSymbolAddress(
  Expected<JITEvaluatedSymbol>* symbolPtr,
  char** errMsg) {
    auto& symbol = *symbolPtr;
    *errMsg = nullptr;
    if (symbol) {
        return symbol->getAddress();
    } else {
        *errMsg = strdup(toString(symbol.takeError()).c_str());
        return 0;
    }
}

LLVMJITSymbolFlags_ LLVM_Hs_getExpectedJITEvaluatedSymbolFlags(
  Expected<JITEvaluatedSymbol>* symbolPtr) {
    auto& symbol = *symbolPtr;
    if (!symbol) {
        return LLVMJITSymbolFlagHasError;
    }
    return wrap(symbol->getFlags());
}

JITEvaluatedSymbol* LLVM_Hs_createJITEvaluatedSymbol(uint64_t ptr, LLVMJITSymbolFlags_ flags) {
    return new JITEvaluatedSymbol(ptr, unwrap(flags));
}

void LLVM_Hs_disposeJITEvaluatedSymbol(JITEvaluatedSymbol* symbol) {
    delete symbol;
}

void LLVM_Hs_disposeExpectedJITEvaluatedSymbol(Expected<JITEvaluatedSymbol>* symbol) {
    delete symbol;
}

MangleAndInterner* LLVM_Hs_createMangleAndInterner(ExecutionSession* es,
                                                   LLVMTargetDataRef dl) {
    return new MangleAndInterner(*es, *unwrap(dl));
}

void LLVM_Hs_disposeMangleAndInterner(MangleAndInterner* mangler) {
    delete mangler;
}

SymbolStringPtr* LLVM_Hs_MangleAndInterner_call(MangleAndInterner* mangler, const char* name) {
    return new SymbolStringPtr((*mangler)(name));
}

const char* LLVM_Hs_SymbolStringPtr_c_str(SymbolStringPtr* ptr) {
    return (*(*ptr)).data();
}

void LLVM_Hs_disposeSymbolStringPtr(SymbolStringPtr* ptr) {
    delete ptr;
}

}
