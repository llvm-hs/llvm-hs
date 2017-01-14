#include "LLVM/Internal/FFI/OrcJIT.h"
#include "LLVM/Internal/FFI/Target.hpp"
#include "llvm/ExecutionEngine/Orc/CompileOnDemandLayer.h"
#include "llvm/ExecutionEngine/Orc/CompileUtils.h"
#include "llvm/ExecutionEngine/Orc/IRCompileLayer.h"
#include "llvm/ExecutionEngine/Orc/IndirectionUtils.h"
#include "llvm/ExecutionEngine/JITSymbol.h"
#include "llvm/ExecutionEngine/Orc/LambdaResolver.h"
#include "llvm/ExecutionEngine/Orc/ObjectLinkingLayer.h"
#include "llvm/IR/Mangler.h"

#include <type_traits>

using namespace llvm;
using namespace orc;

typedef llvm::orc::ObjectLinkingLayer<> *LLVMObjectLinkingLayerRef;
typedef llvm::orc::IRCompileLayer<llvm::orc::ObjectLinkingLayer<>>
    LLVMIRCompileLayer;
typedef LLVMIRCompileLayer *LLVMIRCompileLayerRef;
typedef llvm::orc::CompileOnDemandLayer<LLVMIRCompileLayer>
    LLVMCompileOnDemandLayer;
typedef LLVMCompileOnDemandLayer *LLVMCompileOnDemandLayerRef;
typedef llvm::orc::JITCompileCallbackManager *LLVMJITCompileCallbackManagerRef;
typedef llvm::JITSymbol *LLVMJITSymbolRef;
typedef LLVMIRCompileLayer::ModuleSetHandleT *LLVMModuleSetHandleRef;
typedef LLVMCompileOnDemandLayer::ModuleSetHandleT *LLVMCODModuleSetHandleRef;
typedef llvm::orc::LambdaResolver<
    std::function<JITSymbol(const std::string &name)>,
    std::function<JITSymbol(const std::string &name)>>
    LLVMLambdaResolver;
typedef LLVMLambdaResolver *LLVMLambdaResolverRef;
typedef llvm::orc::IndirectStubsManager *LLVMIndirectStubsManagerRef;
typedef std::function<std::unique_ptr<llvm::orc::IndirectStubsManager>()>
    *LLVMIndirectStubsManagerBuilderRef;

static std::string mangle(StringRef name, LLVMTargetDataRef dataLayout) {
    std::string mangledName;
    {
        raw_string_ostream mangledNameStream(mangledName);
        Mangler::getNameWithPrefix(mangledNameStream, name,
                                   *unwrap(dataLayout));
    }
    return mangledName;
}

static std::vector<Module *> getModules(LLVMModuleRef *modules,
                                        unsigned moduleCount,
                                        LLVMTargetDataRef dataLayout) {
    std::vector<Module *> moduleVec(moduleCount);
    for (unsigned i = 0; i < moduleCount; ++i) {
        moduleVec.at(i) = unwrap(modules[i]);
        if (moduleVec.at(i)->getDataLayout().isDefault()) {
            moduleVec.at(i)->setDataLayout(*unwrap(dataLayout));
        }
    }
    return moduleVec;
}

extern "C" {
LLVMIRCompileLayerRef
LLVM_Hs_createIRCompileLayer(LLVMObjectLinkingLayerRef objectLayer,
                                  LLVMTargetMachineRef tm) {
    TargetMachine *tmm = unwrap(tm);
    return new IRCompileLayer<ObjectLinkingLayer<>>(*objectLayer,
                                                    SimpleCompiler(*tmm));
}

void LLVM_Hs_disposeIRCompileLayer(LLVMIRCompileLayerRef compileLayer) {
    delete compileLayer;
}

LLVMJITSymbolRef
LLVM_Hs_IRCompileLayer_findSymbol(LLVMIRCompileLayerRef compileLayer,
                                       const char *name,
                                       LLVMBool exportedSymbolsOnly) {
    JITSymbol symbol = compileLayer->findSymbol(name, exportedSymbolsOnly);
    return new JITSymbol(symbol);
}

void LLVM_Hs_disposeJITSymbol(LLVMJITSymbolRef symbol) { delete symbol; }

LLVMLambdaResolverRef LLVM_Hs_createLambdaResolver(
    void (*dylibResolver)(const char *, LLVMJITSymbolRef),
    void (*externalResolver)(const char *, LLVMJITSymbolRef)) {
    std::function<JITSymbol(const std::string &name)>
        dylibResolverFun = [dylibResolver](
            const std::string &name) -> JITSymbol {
        JITSymbol symbol(nullptr);
        dylibResolver(name.c_str(), &symbol);
        return symbol;
    };
    std::function<JITSymbol(const std::string &name)>
        externalResolverFun = [externalResolver](
            const std::string &name) -> JITSymbol {
        JITSymbol symbol(nullptr);
        externalResolver(name.c_str(), &symbol);
        return symbol;
    };
    auto lambdaResolver =
        createLambdaResolver(dylibResolverFun, externalResolverFun);
    return lambdaResolver.release();
}

LLVMModuleSetHandleRef LLVM_Hs_IRCompileLayer_addModuleSet(
    LLVMIRCompileLayerRef compileLayer, LLVMTargetDataRef dataLayout,
    LLVMModuleRef *modules, unsigned moduleCount,
    LLVMLambdaResolverRef resolver) {
    std::vector<Module *> moduleVec =
        getModules(modules, moduleCount, dataLayout);
    return new IRCompileLayer<ObjectLinkingLayer<>>::ModuleSetHandleT(
        compileLayer->addModuleSet(
            moduleVec, make_unique<SectionMemoryManager>(), resolver));
}

void LLVM_Hs_IRCompileLayer_removeModuleSet(
    LLVMIRCompileLayerRef compileLayer,
    LLVMModuleSetHandleRef moduleSetHandle) {
    compileLayer->removeModuleSet(*moduleSetHandle);
    delete moduleSetHandle;
}

LLVMObjectLinkingLayerRef LLVM_Hs_createObjectLinkingLayer() {
    return new ObjectLinkingLayer<>();
}

void LLVM_Hs_disposeObjectLinkingLayer(
    LLVMObjectLinkingLayerRef objectLayer) {
    delete objectLayer;
}

static JITSymbolFlags unwrap(LLVMJITSymbolFlags f) {
    JITSymbolFlags flags = JITSymbolFlags::None;
#define ENUM_CASE(x)                                                           \
    if (f & LLVMJITSymbolFlag##x)                                              \
        flags |= JITSymbolFlags::x;
    LLVM_HS_FOR_EACH_JIT_SYMBOL_FLAG(ENUM_CASE)
#undef ENUM_CASE
    return flags;
}

LLVMJITSymbolFlags wrap(JITSymbolFlags f) {
    unsigned r = 0;
#define ENUM_CASE(x)                                                           \
    if ((char)(f & JITSymbolFlags::x))                                         \
        r |= (unsigned)LLVMJITSymbolFlag##x;
    LLVM_HS_FOR_EACH_JIT_SYMBOL_FLAG(ENUM_CASE)
#undef ENUM_CASE
    return LLVMJITSymbolFlags(r);
}

JITTargetAddress
LLVM_Hs_JITSymbol_getAddress(LLVMJITSymbolRef symbol) {
    return symbol->getAddress();
}

LLVMJITSymbolFlags LLVM_Hs_JITSymbol_getFlags(LLVMJITSymbolRef symbol) {
    return wrap(symbol->getFlags());
}

void LLVM_Hs_setJITSymbol(LLVMJITSymbolRef symbol,
                               JITTargetAddress addr,
                               LLVMJITSymbolFlags flags) {
    *symbol = JITSymbol(addr, unwrap(flags));
}

void LLVM_Hs_getMangledSymbol(char **mangledSymbol, const char *symbol,
                                   LLVMTargetDataRef dataLayout) {
    std::string mangled = mangle(symbol, dataLayout);
    *mangledSymbol = new char[mangled.size() + 1];
    strcpy(*mangledSymbol, mangled.c_str());
}

void LLVM_Hs_disposeMangledSymbol(char *mangledSymbol) {
    delete[] mangledSymbol;
}

LLVMJITCompileCallbackManagerRef LLVM_Hs_createLocalCompileCallbackManager(
    const char *triple, JITTargetAddress errorHandler) {
    return llvm::orc::createLocalCompileCallbackManager(Triple(triple),
                                                        errorHandler)
        .release();
}

void LLVM_Hs_disposeCallbackManager(
    LLVMJITCompileCallbackManagerRef callbackManager) {
    delete callbackManager;
}

LLVMIndirectStubsManagerBuilderRef
LLVM_Hs_createLocalIndirectStubsManagerBuilder(const char *triple) {
    return new std::function<std::unique_ptr<IndirectStubsManager>()>(
        llvm::orc::createLocalIndirectStubsManagerBuilder(Triple(triple)));
}

void LLVM_Hs_disposeIndirectStubsManagerBuilder(
    LLVMIndirectStubsManagerBuilderRef stubsManager) {
    delete stubsManager;
}

void LLVM_Hs_insertFun(std::set<llvm::Function *> *set,
                            llvm::Function *f) {
    set->insert(f);
}

LLVMCompileOnDemandLayerRef LLVM_Hs_createCompileOnDemandLayer(
    LLVMIRCompileLayerRef compileLayer,
    void (*partitioningFtor)(llvm::Function *, std::set<llvm::Function *> *set),
    LLVMJITCompileCallbackManagerRef callbackManager,
    LLVMIndirectStubsManagerBuilderRef stubsManager,
    LLVMBool cloneStubsIntoPartitions) {
    return new LLVMCompileOnDemandLayer(
        *compileLayer,
        [partitioningFtor](llvm::Function &f) -> std::set<llvm::Function *> {
            std::set<llvm::Function *> result;
            partitioningFtor(&f, &result);
            return result;
        },
        *callbackManager, *stubsManager, cloneStubsIntoPartitions);
}

void LLVM_Hs_disposeCompileOnDemandLayer(
    LLVMCompileOnDemandLayerRef codLayer) {
    delete codLayer;
}

LLVMCODModuleSetHandleRef LLVM_Hs_CompileOnDemandLayer_addModuleSet(
    LLVMCompileOnDemandLayerRef compileLayer, LLVMTargetDataRef dataLayout,
    LLVMModuleRef *modules, unsigned moduleCount,
    LLVMLambdaResolverRef resolver) {
    std::vector<Module *> moduleVec =
        getModules(modules, moduleCount, dataLayout);
    // We need to copy the resolver to make the use of unique_ptr (required by
    // the LLVM API) safe
    std::unique_ptr<LLVMLambdaResolver> uniqueResolver(
        new LLVMLambdaResolver(*resolver));
    return new LLVMCompileOnDemandLayer::ModuleSetHandleT(
        compileLayer->addModuleSet(moduleVec,
                                   make_unique<SectionMemoryManager>(),
                                   std::move(uniqueResolver)));
}

void LLVM_Hs_CompileOnDemandLayer_removeModuleSet(
    LLVMCompileOnDemandLayerRef compileLayer,
    LLVMCODModuleSetHandleRef moduleSetHandle) {
    compileLayer->removeModuleSet(*moduleSetHandle);
    delete moduleSetHandle;
}

LLVMJITSymbolRef LLVM_Hs_CompileOnDemandLayer_findSymbol(
    LLVMCompileOnDemandLayerRef compileLayer, const char *name,
    LLVMBool exportedSymbolsOnly) {
    JITSymbol symbol = compileLayer->findSymbol(name, exportedSymbolsOnly);
    return new JITSymbol(symbol);
}
}
