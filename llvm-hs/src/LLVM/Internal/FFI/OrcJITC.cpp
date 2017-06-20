#include "LLVM/Internal/FFI/OrcJIT.h"
#include "LLVM/Internal/FFI/Target.hpp"
#include "llvm/ExecutionEngine/JITSymbol.h"
#include "llvm/ExecutionEngine/Orc/CompileOnDemandLayer.h"
#include "llvm/ExecutionEngine/Orc/CompileUtils.h"
#include "llvm/ExecutionEngine/Orc/IRCompileLayer.h"
#include "llvm/ExecutionEngine/Orc/IRTransformLayer.h"
#include "llvm/ExecutionEngine/Orc/IndirectionUtils.h"
#include "llvm/ExecutionEngine/Orc/LambdaResolver.h"
#include "llvm/ExecutionEngine/Orc/ObjectLinkingLayer.h"
#include "llvm/IR/Mangler.h"

#include <type_traits>
#include <unordered_map>

using namespace llvm;
using namespace orc;

typedef unsigned LLVMModuleSetHandle;
typedef unsigned LLVMObjSetHandle;
typedef llvm::orc::LambdaResolver<
    std::function<JITSymbol(const std::string &name)>,
    std::function<JITSymbol(const std::string &name)>>
    LLVMLambdaResolver;
typedef LLVMLambdaResolver *LLVMLambdaResolverRef;

// We want to allow users to choose themselves which layers they want to use.
// However, the LLVM API requires that this is selected statically via template
// arguments. We convert this static polymorphism to runtime polymorphism by
// creating an LinkingLayer and a CompileLayer class which use virtual dispatch
// to select the concrete layer.

template <typename T> class HandleSet {
  public:
    unsigned insert(T t) {
        unsigned handle = handles.insert({nextFree, t}).first->first;
        ++nextFree;
        return handle;
    }
    T &lookup(unsigned i) { return handles.at(i); }

    void remove(unsigned i) { handles.erase(i); }

  private:
    std::unordered_map<unsigned, T> handles;
    unsigned nextFree = 0;
};

class LinkingLayer {
  public:
    virtual ~LinkingLayer(){};
    typedef unsigned ObjSetHandleT;
    virtual ObjSetHandleT
    addObjectSet(std::vector<std::unique_ptr<object::ObjectFile>> objects,
                 SectionMemoryManager *memMgr, JITSymbolResolver *resolver) = 0;
    virtual ObjSetHandleT addObjectSet(
        std::vector<std::unique_ptr<object::OwningBinary<object::ObjectFile>>>
            objects,
        SectionMemoryManager *memMgr, JITSymbolResolver *resolver) = 0;
    virtual ObjSetHandleT addObjectSet(
        std::vector<std::unique_ptr<object::OwningBinary<object::ObjectFile>>>
            objects,
        std::unique_ptr<SectionMemoryManager> memMgr,
        JITSymbolResolver *resolver) = 0;
    virtual ObjSetHandleT addObjectSet(
        std::vector<std::unique_ptr<object::OwningBinary<object::ObjectFile>>>
            objects,
        std::unique_ptr<SectionMemoryManager> memMgr,
        std::unique_ptr<JITSymbolResolver> resolver) = 0;
    virtual ObjSetHandleT addObjectSet(
        std::vector<std::unique_ptr<object::OwningBinary<object::ObjectFile>>>
            objects,
        SectionMemoryManager *memMgr,
        std::unique_ptr<JITSymbolResolver> resolver) = 0;
    virtual void removeObjectSet(ObjSetHandleT H) = 0;
    virtual JITSymbol findSymbol(StringRef name, bool exportedSymbolsOnly) = 0;
    virtual JITSymbol findSymbolIn(ObjSetHandleT h, StringRef name,
                                   bool exportedSymbolsOnly) = 0;
    virtual void emitAndFinalize(ObjSetHandleT h) = 0;
};

template <typename T> class LinkingLayerT : public LinkingLayer {
  public:
    LinkingLayerT(T data_) : data(std::move(data_)) {}
    ObjSetHandleT
    addObjectSet(std::vector<std::unique_ptr<object::ObjectFile>> objects,
                 SectionMemoryManager *memMgr,
                 JITSymbolResolver *resolver) override {
        auto handle = data.addObjectSet(std::move(objects), std::move(memMgr),
                                        std::move(resolver));
        return handles.insert(handle);
    }
    ObjSetHandleT addObjectSet(
        std::vector<std::unique_ptr<object::OwningBinary<object::ObjectFile>>>
            objects,
        SectionMemoryManager *memMgr, JITSymbolResolver *resolver) override {
        auto handle = data.addObjectSet(std::move(objects), std::move(memMgr),
                                        std::move(resolver));
        return handles.insert(handle);
    }
    ObjSetHandleT addObjectSet(
        std::vector<std::unique_ptr<object::OwningBinary<object::ObjectFile>>>
            objects,
        std::unique_ptr<SectionMemoryManager> memMgr,
        JITSymbolResolver *resolver) override {
        auto handle = data.addObjectSet(std::move(objects), std::move(memMgr),
                                        std::move(resolver));
        return handles.insert(handle);
    }
    ObjSetHandleT addObjectSet(
        std::vector<std::unique_ptr<object::OwningBinary<object::ObjectFile>>>
            objects,
        std::unique_ptr<SectionMemoryManager> memMgr,
        std::unique_ptr<JITSymbolResolver> resolver) override {
        auto handle = data.addObjectSet(std::move(objects), std::move(memMgr),
                                        std::move(resolver));
        return handles.insert(handle);
    }
    ObjSetHandleT addObjectSet(
        std::vector<std::unique_ptr<object::OwningBinary<object::ObjectFile>>>
            objects,
        SectionMemoryManager *memMgr,
        std::unique_ptr<JITSymbolResolver> resolver) override {
        auto handle = data.addObjectSet(std::move(objects), std::move(memMgr),
                                        std::move(resolver));
        return handles.insert(handle);
    }
    void removeObjectSet(ObjSetHandleT h) override {
        data.removeObjectSet(handles.lookup(h));
        handles.remove(h);
    }
    JITSymbol findSymbol(StringRef name, bool exportedSymbolsOnly) override {
        return data.findSymbol(name, exportedSymbolsOnly);
    }
    JITSymbol findSymbolIn(ObjSetHandleT h, StringRef name,
                           bool exportedSymbolsOnly) override {
        return data.findSymbolIn(handles.lookup(h), name, exportedSymbolsOnly);
    }
    void emitAndFinalize(ObjSetHandleT h) override {
        data.emitAndFinalize(handles.lookup(h));
    }

  private:
    T data;
    HandleSet<typename T::ObjSetHandleT> handles;
};

class CompileLayer {
  public:
    typedef LLVMModuleSetHandle ModuleSetHandleT;
    virtual ~CompileLayer(){};
    virtual JITSymbol findSymbol(StringRef Name, bool ExportedSymbolsOnly) = 0;
    virtual JITSymbol findSymbolIn(ModuleSetHandleT H, StringRef Name,
                                   bool ExportedSymbolsOnly) = 0;
    virtual ModuleSetHandleT
    addModuleSet(std::vector<std::unique_ptr<Module>> Modules,
                 std::unique_ptr<SectionMemoryManager> MemMgr,
                 std::unique_ptr<JITSymbolResolver> Resolver) = 0;
    virtual ModuleSetHandleT
    addModuleSet(std::vector<std::unique_ptr<Module>> Modules,
                 SectionMemoryManager *MemMgr,
                 std::unique_ptr<JITSymbolResolver> Resolver) = 0;
    virtual void removeModuleSet(ModuleSetHandleT H) = 0;
};

template <typename T> class CompileLayerT : public CompileLayer {
  public:
    CompileLayerT(T data_) : data(std::move(data_)) {}
    JITSymbol findSymbol(StringRef Name, bool ExportedSymbolsOnly) override {
        return data.findSymbol(Name, ExportedSymbolsOnly);
    }
    JITSymbol findSymbolIn(ModuleSetHandleT H, StringRef Name,
                           bool ExportedSymbolsOnly) override {
        return data.findSymbolIn(handles.lookup(H), Name, ExportedSymbolsOnly);
    }
    ModuleSetHandleT
    addModuleSet(std::vector<std::unique_ptr<Module>> Modules,
                 std::unique_ptr<SectionMemoryManager> MemMgr,
                 std::unique_ptr<JITSymbolResolver> Resolver) override {
        auto handle = data.addModuleSet(std::move(Modules), std::move(MemMgr),
                                        std::move(Resolver));
        return handles.insert(handle);
    }
    ModuleSetHandleT
    addModuleSet(std::vector<std::unique_ptr<Module>> Modules,
                 SectionMemoryManager *MemMgr,
                 std::unique_ptr<JITSymbolResolver> Resolver) override {
        auto handle = data.addModuleSet(std::move(Modules), std::move(MemMgr),
                                        std::move(Resolver));
        return handles.insert(handle);
    }
    void removeModuleSet(ModuleSetHandleT H) override {
        data.removeModuleSet(handles.lookup(H));
        handles.remove(H);
    }

  private:
    T data;
    HandleSet<typename T::ModuleSetHandleT> handles;
};

typedef llvm::orc::CompileOnDemandLayer<CompileLayer> LLVMCompileOnDemandLayer;
typedef LLVMCompileOnDemandLayer *LLVMCompileOnDemandLayerRef;

typedef llvm::orc::IRTransformLayer<
    CompileLayer,
    std::function<std::unique_ptr<Module>(std::unique_ptr<Module>)>>
    LLVMIRTransformLayer;

typedef llvm::orc::JITCompileCallbackManager *LLVMJITCompileCallbackManagerRef;

typedef llvm::JITSymbol *LLVMJITSymbolRef;

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

static std::vector<std::unique_ptr<Module>>
getModules(LLVMModuleRef *modules, unsigned moduleCount,
           LLVMTargetDataRef dataLayout) {
    std::vector<std::unique_ptr<Module>> moduleVec(moduleCount);
    for (unsigned i = 0; i < moduleCount; ++i) {
        moduleVec.at(i) = std::unique_ptr<Module>(unwrap(modules[i]));
        if (moduleVec.at(i)->getDataLayout().isDefault()) {
            moduleVec.at(i)->setDataLayout(*unwrap(dataLayout));
        }
    }
    return moduleVec;
}

extern "C" {

/* Constructor functions for the different compile layers */

CompileLayer *LLVM_Hs_createIRCompileLayer(LinkingLayer *linkingLayer,
                                           LLVMTargetMachineRef tm) {
    TargetMachine *tmm = unwrap(tm);
    return new CompileLayerT<IRCompileLayer<LinkingLayer>>(
        IRCompileLayer<LinkingLayer>(*linkingLayer, SimpleCompiler(*tmm)));
}

CompileLayer *LLVM_Hs_createCompileOnDemandLayer(
    CompileLayer *compileLayer,
    void (*partitioningFtor)(llvm::Function *, std::set<llvm::Function *> *set),
    LLVMJITCompileCallbackManagerRef callbackManager,
    LLVMIndirectStubsManagerBuilderRef stubsManager,
    LLVMBool cloneStubsIntoPartitions) {
    return new CompileLayerT<LLVMCompileOnDemandLayer>(LLVMCompileOnDemandLayer(
        *compileLayer,
        [partitioningFtor](llvm::Function &f) -> std::set<llvm::Function *> {
            std::set<llvm::Function *> result;
            partitioningFtor(&f, &result);
            return result;
        },
        *callbackManager, *stubsManager, cloneStubsIntoPartitions));
}

CompileLayer *LLVM_Hs_createIRTransformLayer(CompileLayer *compileLayer,
                                             Module *(*transform)(Module *)) {
    std::function<std::unique_ptr<Module>(std::unique_ptr<Module>)> transform_ =
        [transform](std::unique_ptr<Module> module) {
            return std::unique_ptr<Module>(transform(module.release()));
        };
    return new CompileLayerT<LLVMIRTransformLayer>(
        LLVMIRTransformLayer(*compileLayer, transform_));
}

/* Functions that work on all compile layers */

void LLVM_Hs_CompileLayer_dispose(CompileLayer *compileLayer) {
    delete compileLayer;
}

LLVMJITSymbolRef LLVM_Hs_CompileLayer_findSymbol(CompileLayer *compileLayer,
                                                 const char *name,
                                                 LLVMBool exportedSymbolsOnly) {
    JITSymbol symbol = compileLayer->findSymbol(name, exportedSymbolsOnly);
    return new JITSymbol(symbol);
}

LLVMJITSymbolRef
LLVM_Hs_CompileLayer_findSymbolIn(CompileLayer *compileLayer,
                                  LLVMModuleSetHandle handle, const char *name,
                                  LLVMBool exportedSymbolsOnly) {
    JITSymbol symbol =
        compileLayer->findSymbolIn(handle, name, exportedSymbolsOnly);
    return new JITSymbol(symbol);
}

LLVMModuleSetHandle
LLVM_Hs_CompileLayer_addModuleSet(CompileLayer *compileLayer,
                                  LLVMTargetDataRef dataLayout,
                                  LLVMModuleRef *modules, unsigned moduleCount,
                                  LLVMLambdaResolverRef resolver) {
    auto moduleVec = getModules(modules, moduleCount, dataLayout);
    std::unique_ptr<LLVMLambdaResolver> uniqueResolver(
        new LLVMLambdaResolver(*resolver));
    return compileLayer->addModuleSet(std::move(moduleVec),
                                      make_unique<SectionMemoryManager>(),
                                      std::move(uniqueResolver));
}

void LLVM_Hs_CompileLayer_removeModuleSet(CompileLayer *compileLayer,
                                          LLVMModuleSetHandle moduleSetHandle) {
    compileLayer->removeModuleSet(moduleSetHandle);
}

/* Constructor functions for the different object layers */

LinkingLayer *LLVM_Hs_createObjectLinkingLayer() {
    return new LinkingLayerT<ObjectLinkingLayer<>>(ObjectLinkingLayer<>());
}

/* Fuctions that work on all object layers */

void LLVM_Hs_LinkingLayer_dispose(LinkingLayer *linkingLayer) {
    delete linkingLayer;
}

void LLVM_Hs_disposeJITSymbol(LLVMJITSymbolRef symbol) { delete symbol; }

LLVMLambdaResolverRef LLVM_Hs_createLambdaResolver(
    void (*dylibResolver)(const char *, LLVMJITSymbolRef),
    void (*externalResolver)(const char *, LLVMJITSymbolRef)) {
    std::function<JITSymbol(const std::string &name)> dylibResolverFun =
        [dylibResolver](const std::string &name) -> JITSymbol {
        JITSymbol symbol(nullptr);
        dylibResolver(name.c_str(), &symbol);
        return symbol;
    };
    std::function<JITSymbol(const std::string &name)> externalResolverFun =
        [externalResolver](const std::string &name) -> JITSymbol {
        JITSymbol symbol(nullptr);
        externalResolver(name.c_str(), &symbol);
        return symbol;
    };
    auto lambdaResolver =
        createLambdaResolver(dylibResolverFun, externalResolverFun);
    return lambdaResolver.release();
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

static LLVMJITSymbolFlags wrap(JITSymbolFlags f) {
    unsigned r = 0;
#define ENUM_CASE(x)                                                           \
    if ((char)(f & JITSymbolFlags::x))                                         \
        r |= (unsigned)LLVMJITSymbolFlag##x;
    LLVM_HS_FOR_EACH_JIT_SYMBOL_FLAG(ENUM_CASE)
#undef ENUM_CASE
    return LLVMJITSymbolFlags(r);
}

JITTargetAddress LLVM_Hs_JITSymbol_getAddress(LLVMJITSymbolRef symbol) {
    return symbol->getAddress();
}

LLVMJITSymbolFlags LLVM_Hs_JITSymbol_getFlags(LLVMJITSymbolRef symbol) {
    return wrap(symbol->getFlags());
}

void LLVM_Hs_setJITSymbol(LLVMJITSymbolRef symbol, JITTargetAddress addr,
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

LLVMJITCompileCallbackManagerRef
LLVM_Hs_createLocalCompileCallbackManager(const char *triple,
                                          JITTargetAddress errorHandler) {
    // We copy the string so that it can be freed on the Haskell side.
    std::string tripleStr(triple);
    return llvm::orc::createLocalCompileCallbackManager(
               Triple(std::move(tripleStr)), errorHandler)
        .release();
}

void LLVM_Hs_disposeCallbackManager(
    LLVMJITCompileCallbackManagerRef callbackManager) {
    delete callbackManager;
}

LLVMIndirectStubsManagerBuilderRef
LLVM_Hs_createLocalIndirectStubsManagerBuilder(const char *triple) {
    // We copy the string so that it can be freed on the Haskell side.
    std::string tripleStr(triple);
    return new std::function<std::unique_ptr<IndirectStubsManager>()>(
        llvm::orc::createLocalIndirectStubsManagerBuilder(
            Triple(std::move(tripleStr))));
}

void LLVM_Hs_disposeIndirectStubsManagerBuilder(
    LLVMIndirectStubsManagerBuilderRef stubsManager) {
    delete stubsManager;
}

void LLVM_Hs_insertFun(std::set<llvm::Function *> *set, llvm::Function *f) {
    set->insert(f);
}
}
