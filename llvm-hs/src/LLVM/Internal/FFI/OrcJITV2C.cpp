#include <iostream>

#include <llvm/ExecutionEngine/Orc/CompileUtils.h>
#include <llvm/ExecutionEngine/Orc/Core.h>
#include <llvm/ExecutionEngine/Orc/IRCompileLayer.h>
#include <llvm/ExecutionEngine/Orc/RTDyldObjectLinkingLayer.h>
#include <llvm/ExecutionEngine/SectionMemoryManager.h>
#include <llvm/IR/Module.h>

#include "LLVM/Internal/FFI/Target.hpp"

using namespace llvm;
using namespace orc;

extern "C" {

// Thread-safe context

ThreadSafeContext* LLVM_Hs_createThreadSafeContext() {
    return new ThreadSafeContext(llvm::make_unique<LLVMContext>());
}

void LLVM_Hs_disposeThreadSafeContext(ThreadSafeContext* ctx) {
    delete ctx;
}

// Object layer

ObjectLayer* LLVM_Hs_createRTDyldObjectLinkingLayer(ExecutionSession* es) {
    return new RTDyldObjectLinkingLayer(*es, []() { return llvm::make_unique<SectionMemoryManager>(); });
}

void LLVM_Hs_disposeObjectLayer(ObjectLayer* ol) {
    delete ol;
}

// Compile layer

IRLayer* LLVM_Hs_createIRCompileLayer(ExecutionSession* es, ObjectLayer* baseLayer, LLVMTargetMachineRef tm) {
    return new IRCompileLayer(*es, *baseLayer, SimpleCompiler(*unwrap(tm)));
}

void LLVM_Hs_disposeIRLayer(IRLayer* il) {
    delete il;
}

// Warning: This consumes the module.
void LLVM_Hs_IRLayer_add(ThreadSafeContext*  ctx, ExecutionSession* es, LLVMTargetDataRef dataLayout, IRLayer* il, LLVMModuleRef m) {
    std::unique_ptr<Module> mod{unwrap(m)};
    if (mod->getDataLayout().isDefault()) {
        mod->setDataLayout(*unwrap(dataLayout));
    }
    if (Error err = il->add(es->getMainJITDylib(), ThreadSafeModule(std::move(mod), *ctx))) {
        llvm::errs() << err << "\n";
        exit(1);
    }
}

uint64_t LLVM_Hs_ExecutionSession_lookup(ExecutionSession* es, const char* mangledName) {
    if (auto symbolOrErr = es->lookup({&es->getMainJITDylib()}, mangledName)) {
        auto& symbol = *symbolOrErr;
        return symbol.getAddress();
    } else {
        Error err = symbolOrErr.takeError();
        llvm::errs() << err << "\n";
        exit(1);
    }
}

}
