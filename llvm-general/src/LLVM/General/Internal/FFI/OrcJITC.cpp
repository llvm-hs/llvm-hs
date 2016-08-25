#include "LLVM/General/Internal/FFI/OrcJIT.hpp"

#include "llvm/ExecutionEngine/Orc/CompileUtils.h"
#include "llvm/ExecutionEngine/Orc/LambdaResolver.h"

#include "llvm/IR/Mangler.h"

#include <type_traits>

using namespace llvm;
using namespace orc;

static std::string mangle(StringRef name, LLVMTargetDataRef dataLayout) {
    std::string mangledName;
    {
        raw_string_ostream mangledNameStream(mangledName);
        Mangler::getNameWithPrefix(mangledNameStream, name,
                                   *unwrap(dataLayout));
    }
    return mangledName;
}

LLVMIRCompileLayerRef
LLVM_General_createIRCompileLayer(LLVMObjectLinkingLayerRef objectLayer,
                                  LLVMTargetMachineRef tm) {
    TargetMachine *tmm = unwrap(tm);
    return new IRCompileLayer<ObjectLinkingLayer<>>(*objectLayer,
                                                    SimpleCompiler(*tmm));
}

void LLVM_General_disposeIRCompileLayer(LLVMIRCompileLayerRef compileLayer) {
    delete compileLayer;
}

LLVMJITSymbolRef LLVM_General_IRCompileLayer_findSymbol(
    LLVMIRCompileLayerRef compileLayer, LLVMTargetDataRef dataLayout,
    const char *name, LLVMBool exportedSymbolsOnly) {
    JITSymbol symbol = compileLayer->findSymbol(name, exportedSymbolsOnly);
    return new JITSymbol(symbol);
}

void LLVM_General_disposeJITSymbol(LLVMJITSymbolRef symbol) { delete symbol; }

LLVMModuleSetHandleRef LLVM_General_IRCompileLayer_addModuleSet(
    LLVMIRCompileLayerRef compileLayer, LLVMTargetDataRef dataLayout,
    LLVMModuleRef *modules, unsigned moduleCount,
    void (*dylibResolver)(const char *, LLVMJITSymbolRef),
    void (*externalResolver)(const char *, LLVMJITSymbolRef)) {
    auto lambdaResolver = createLambdaResolver(
        [dylibResolver,
         externalResolver](const std::string &name) -> RuntimeDyld::SymbolInfo {
            JITSymbol symbol(nullptr);
            dylibResolver(name.c_str(), &symbol);
            return symbol.toRuntimeDyldSymbol();
        },
        [&](const std::string &name) -> RuntimeDyld::SymbolInfo {
            JITSymbol symbol(nullptr);
            externalResolver(name.c_str(), &symbol);
            return symbol.toRuntimeDyldSymbol();
        });
    std::vector<Module *> moduleVec(moduleCount);
    for (unsigned i = 0; i < moduleCount; ++i) {
        moduleVec.at(i) = unwrap(modules[i]);
        if (moduleVec.at(i)->getDataLayout().isDefault()) {
            moduleVec.at(i)->setDataLayout(*unwrap(dataLayout));
        }
    }
    return new IRCompileLayer<ObjectLinkingLayer<>>::ModuleSetHandleT(
        compileLayer->addModuleSet(moduleVec,
                                   make_unique<SectionMemoryManager>(),
                                   std::move(lambdaResolver)));
}

void LLVM_General_IRCompileLayer_removeModuleSet(
    LLVMIRCompileLayerRef compileLayer,
    LLVMModuleSetHandleRef moduleSetHandle) {
    compileLayer->removeModuleSet(*moduleSetHandle);
    delete moduleSetHandle;
}

LLVMObjectLinkingLayerRef LLVM_General_createObjectLinkingLayer() {
    return new ObjectLinkingLayer<>();
}

void LLVM_General_disposeObjectLinkingLayer(
    LLVMObjectLinkingLayerRef objectLayer) {
    delete objectLayer;
}

static JITSymbolFlags unwrap(LLVMJITSymbolFlags f) {
    JITSymbolFlags flags = JITSymbolFlags::None;
#define ENUM_CASE(x)                                                           \
    if (f & LLVMJITSymbolFlag##x)                                              \
        flags |= JITSymbolFlags::x;
    LLVM_GENERAL_FOR_EACH_JIT_SYMBOL_FLAG(ENUM_CASE)
#undef ENUM_CASE
    return flags;
}

LLVMJITSymbolFlags wrap(JITSymbolFlags f) {
    unsigned r = 0;
#define ENUM_CASE(x)                                                           \
    if ((char)(f & JITSymbolFlags::x))                                         \
        r |= (unsigned)LLVMJITSymbolFlag##x;
    LLVM_GENERAL_FOR_EACH_JIT_SYMBOL_FLAG(ENUM_CASE)
#undef ENUM_CASE
    return LLVMJITSymbolFlags(r);
}

llvm::orc::TargetAddress
LLVM_General_JITSymbol_getAddress(LLVMJITSymbolRef symbol) {
    return symbol->getAddress();
}

LLVMJITSymbolFlags LLVM_General_JITSymbol_getFlags(LLVMJITSymbolRef symbol) {
    return wrap(symbol->getFlags());
}

void LLVM_General_setJITSymbol(LLVMJITSymbolRef symbol,
                               llvm::orc::TargetAddress addr,
                               LLVMJITSymbolFlags flags) {
    *symbol = JITSymbol(addr, unwrap(flags));
}

void LLVM_General_getMangledSymbol(char **mangledSymbol, const char *symbol,
                                   LLVMTargetDataRef dataLayout) {
    std::string mangled = mangle(symbol, dataLayout);
    *mangledSymbol = new char[mangled.size() + 1];
    strcpy(*mangledSymbol, mangled.c_str());
}

void LLVM_General_disposeMangledSymbol(char *mangledSymbol) {
    delete[] mangledSymbol;
}
