// We ignore deprecations until we get around to updating to the new OrcJIT API
#pragma GCC diagnostic ignored "-Wdeprecated-declarations"
#pragma clang diagnostic ignored "-Wdeprecated-declarations"

#include "llvm/Support/Error.h"

#include "LLVM/Internal/FFI/ErrorHandling.hpp"
#include "LLVM/Internal/FFI/OrcJIT.h"
#include "LLVM/Internal/FFI/Target.hpp"
#include "llvm/ExecutionEngine/JITSymbol.h"
#include "llvm/ExecutionEngine/Orc/CompileOnDemandLayer.h"
#include "llvm/ExecutionEngine/Orc/CompileUtils.h"
#include "llvm/ExecutionEngine/Orc/IRCompileLayer.h"
#include "llvm/ExecutionEngine/Orc/IRTransformLayer.h"
#include "llvm/ExecutionEngine/Orc/IndirectionUtils.h"
#include "llvm/ExecutionEngine/Orc/LambdaResolver.h"
#include "llvm/ExecutionEngine/Orc/RTDyldObjectLinkingLayer.h"
#include "llvm/ExecutionEngine/SectionMemoryManager.h"
#include "llvm/IR/Mangler.h"

#include <type_traits>
#include <unordered_map>

#include "llvm-c/Object.h"
#include "llvm/ExecutionEngine/Orc/RTDyldObjectLinkingLayer.h"

using namespace llvm;
using namespace orc;

static_assert(std::is_same<VModuleKey, uint64_t>::value,
              "VModuleKey should be uint64_t");

#define SYMBOL_CASE(x)                                                         \
    static_assert((unsigned)LLVMJITSymbolFlag##x ==                            \
                      (unsigned)llvm::JITSymbolFlags::FlagNames::x,            \
                  "JITSymbolFlag values should agree");
LLVM_HS_FOR_EACH_JIT_SYMBOL_FLAG(SYMBOL_CASE)

typedef std::shared_ptr<SymbolResolver> *LLVMSymbolResolverRef;

// We want to allow users to choose themselves which layers they want to use.
// However, the LLVM API requires that this is selected statically via template
// arguments. We convert this static polymorphism to runtime polymorphism by
// creating an LinkingLayer and a CompileLayer class which use virtual dispatch
// to select the concrete layer.

class LinkingLayer {
  public:
    using ObjectPtr = std::unique_ptr<MemoryBuffer>;
    virtual ~LinkingLayer(){};
    virtual Error addObject(VModuleKey k, ObjectPtr objBuffer) = 0;
    virtual Error removeObject(VModuleKey k) = 0;
    virtual JITSymbol findSymbol(StringRef name, bool exportedSymbolsOnly) = 0;
    virtual JITSymbol findSymbolIn(VModuleKey k, StringRef name,
                                   bool exportedSymbolsOnly) = 0;
    virtual Error emitAndFinalize(VModuleKey k) = 0;
};

template <typename T> class LinkingLayerT : public LinkingLayer {
  public:
    LinkingLayerT(T data_) : data(std::move(data_)) {}
    Error addObject(VModuleKey k, ObjectPtr objBuffer) override {
        return data.addObject(k, std::move(objBuffer));
    }
    Error removeObject(VModuleKey k) override { return data.removeObject(k); }
    JITSymbol findSymbol(StringRef name, bool exportedSymbolsOnly) override {
        return data.findSymbol(name, exportedSymbolsOnly);
    }
    JITSymbol findSymbolIn(VModuleKey k, StringRef name,
                           bool exportedSymbolsOnly) override {
        return data.findSymbolIn(k, name, exportedSymbolsOnly);
    }
    Error emitAndFinalize(VModuleKey k) override {
        return data.emitAndFinalize(k);
    }

  private:
    T data;
};

class CompileLayer {
  public:
    virtual ~CompileLayer(){};
    virtual JITSymbol findSymbol(StringRef Name, bool ExportedSymbolsOnly) = 0;
    virtual JITSymbol findSymbolIn(VModuleKey K, StringRef Name,
                                   bool ExportedSymbolsOnly) = 0;
    virtual Error addModule(VModuleKey K, std::unique_ptr<Module> Module) = 0;
    virtual Error removeModule(VModuleKey K) = 0;
};

template <typename T> class CompileLayerT : public CompileLayer {
  public:
    template <typename... Arg>
    CompileLayerT(Arg &&... arg) : data{std::forward<Arg>(arg)...} {}
    JITSymbol findSymbol(StringRef Name, bool ExportedSymbolsOnly) override {
        return data.findSymbol(Name, ExportedSymbolsOnly);
    }
    JITSymbol findSymbolIn(VModuleKey K, StringRef Name,
                           bool ExportedSymbolsOnly) override {
        return data.findSymbolIn(K, Name, ExportedSymbolsOnly);
    }
    Error addModule(VModuleKey K, std::unique_ptr<Module> Module) override {
        return data.addModule(K, std::move(Module));
    }
    Error removeModule(VModuleKey K) override { return data.removeModule(K); }

  private:
    T data;
};

typedef llvm::orc::LegacyCompileOnDemandLayer<CompileLayer> LLVMCompileOnDemandLayer;
typedef LLVMCompileOnDemandLayer *LLVMCompileOnDemandLayerRef;

typedef llvm::orc::LegacyIRTransformLayer<
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

// LLVM doesn’t declare this function in a header so we need to copy it here
static inline object::OwningBinary<object::ObjectFile> *
unwrap(LLVMObjectFileRef OF) {
    return reinterpret_cast<object::OwningBinary<object::ObjectFile> *>(OF);
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
    if (f & JITSymbolFlags::x)                       \
        r |= (unsigned)LLVMJITSymbolFlag##x;
    LLVM_HS_FOR_EACH_JIT_SYMBOL_FLAG(ENUM_CASE)
#undef ENUM_CASE
    return LLVMJITSymbolFlags(r);
}

extern "C" {

ExecutionSession *LLVM_Hs_createExecutionSession() {
    return new ExecutionSession();
}

void LLVM_Hs_disposeExecutionSession(ExecutionSession *es) { delete es; }

VModuleKey LLVM_Hs_allocateVModule(ExecutionSession *es) {
    return es->allocateVModule();
}

void LLVM_Hs_releaseVModule(ExecutionSession *es, VModuleKey k) {
    es->releaseVModule(k);
}

/* Constructor functions for the different compile layers */

CompileLayer *LLVM_Hs_createLegacyIRCompileLayer(LinkingLayer *linkingLayer,
                                                 LLVMTargetMachineRef tm) {
    TargetMachine *tmm = unwrap(tm);
    return new CompileLayerT<LegacyIRCompileLayer<LinkingLayer, SimpleCompiler>>(
        LegacyIRCompileLayer<LinkingLayer, SimpleCompiler>(*linkingLayer,
                                                           SimpleCompiler(*tmm)));
}

CompileLayer *LLVM_Hs_createCompileOnDemandLayer(
    ExecutionSession *es, CompileLayer *compileLayer,
    LLVMSymbolResolverRef (*getSymbolResolver)(VModuleKey k),
    void (*setSymbolResolver)(VModuleKey k, LLVMSymbolResolverRef r),
    void (*partitioningFtor)(llvm::Function *, std::set<llvm::Function *> *set),
    LLVMJITCompileCallbackManagerRef callbackManager,
    LLVMIndirectStubsManagerBuilderRef stubsManager,
    LLVMBool cloneStubsIntoPartitions) {
    std::function<std::shared_ptr<SymbolResolver>(VModuleKey)>
        getSymbolResolverFn =
            [getSymbolResolver](VModuleKey k) { return *getSymbolResolver(k); };
    std::function<void(VModuleKey, std::shared_ptr<SymbolResolver>)>
        setSymbolResolverFn =
            [setSymbolResolver](VModuleKey k,
                                std::shared_ptr<SymbolResolver> r) {
                setSymbolResolver(k, new std::shared_ptr<SymbolResolver>(r));
            };
    return new CompileLayerT<LLVMCompileOnDemandLayer>(
        *es, *compileLayer, getSymbolResolverFn, setSymbolResolverFn,
        [partitioningFtor](llvm::Function &f) -> std::set<llvm::Function *> {
            std::set<llvm::Function *> result;
            partitioningFtor(&f, &result);
            return result;
        },
        *callbackManager, *stubsManager,
        static_cast<bool>(cloneStubsIntoPartitions));
}

CompileLayer *LLVM_Hs_createIRTransformLayer(CompileLayer *compileLayer,
                                             Module *(*transform)(Module *)) {
    std::function<std::unique_ptr<Module>(std::unique_ptr<Module>)> transform_ =
        [transform](std::unique_ptr<Module> module) {
            return std::unique_ptr<Module>(transform(module.release()));
        };
    return new CompileLayerT<LLVMIRTransformLayer>(*compileLayer, transform_);
}

/* Functions that work on all compile layers */

void LLVM_Hs_CompileLayer_dispose(CompileLayer *compileLayer) {
    delete compileLayer;
}

LLVMJITSymbolRef LLVM_Hs_CompileLayer_findSymbol(CompileLayer *compileLayer,
                                                 const char *name,
                                                 LLVMBool exportedSymbolsOnly) {
    JITSymbol symbol = compileLayer->findSymbol(name, exportedSymbolsOnly);
    return new JITSymbol(std::move(symbol));
}

LLVMJITSymbolRef
LLVM_Hs_CompileLayer_findSymbolIn(CompileLayer *compileLayer, VModuleKey k,
                                  const char *name,
                                  LLVMBool exportedSymbolsOnly) {
    JITSymbol symbol = compileLayer->findSymbolIn(k, name, exportedSymbolsOnly);
    return new JITSymbol(std::move(symbol));
}

void LLVM_Hs_CompileLayer_addModule(CompileLayer *compileLayer,
                                    LLVMTargetDataRef dataLayout, VModuleKey k,
                                    LLVMModuleRef module, char **errorMessage) {
    std::unique_ptr<Module> mod{unwrap(module)};
    if (mod->getDataLayout().isDefault()) {
        mod->setDataLayout(*unwrap(dataLayout));
    }
    if (auto err = compileLayer->addModule(k, std::move(mod))) {
        std::string errString = toString(std::move(err));
        *errorMessage = strdup(errString.c_str());
    }
    *errorMessage = nullptr;
}

void LLVM_Hs_CompileLayer_removeModule(CompileLayer *compileLayer,
                                       VModuleKey k) {
    if (compileLayer->removeModule(k)) {
        // TODO handle failure
    }
}

/* Constructor functions for the different object layers */

LinkingLayer *LLVM_Hs_createObjectLinkingLayer(
    ExecutionSession *es, LLVMSymbolResolverRef (*symbolResolver)(VModuleKey)) {
    return new LinkingLayerT<LegacyRTDyldObjectLinkingLayer>(
        LegacyRTDyldObjectLinkingLayer(*es, [symbolResolver](VModuleKey k) {
            return LegacyRTDyldObjectLinkingLayer::Resources{
                std::make_shared<SectionMemoryManager>(), *symbolResolver(k)};
        }));
}

/* Fuctions that work on all object layers */

void LLVM_Hs_LinkingLayer_dispose(LinkingLayer *linkingLayer) {
    delete linkingLayer;
}

void LLVM_Hs_disposeJITSymbol(LLVMJITSymbolRef symbol) { delete symbol; }

LLVMJITSymbolRef LLVM_Hs_LinkingLayer_findSymbol(LinkingLayer *linkingLayer,
                                                 const char *name,
                                                 LLVMBool exportedSymbolsOnly) {
    JITSymbol symbol = linkingLayer->findSymbol(name, exportedSymbolsOnly);
    return new JITSymbol(std::move(symbol));
}

LLVMJITSymbolRef
LLVM_Hs_LinkingLayer_findSymbolIn(LinkingLayer *linkingLayer, VModuleKey k,
                                  const char *name,
                                  LLVMBool exportedSymbolsOnly) {
    JITSymbol symbol = linkingLayer->findSymbolIn(k, name, exportedSymbolsOnly);
    return new JITSymbol(std::move(symbol));
}

LLVMSymbolResolverRef LLVM_Hs_createLambdaResolver(
    ExecutionSession *es,
    void (*rawResolverFn)(const char *, LLVMJITSymbolRef)) {
    std::function<JITSymbol(const std::string &name)> resolverFn =
        [rawResolverFn](const std::string &name) -> JITSymbol {
        JITSymbol symbol(nullptr);
        rawResolverFn(name.c_str(), &symbol);
        return symbol;
    };
    return new std::shared_ptr<SymbolResolver>(
        createLegacyLookupResolver(*es, resolverFn, [](Error err) {
            cantFail(std::move(err), "lookupFlags failed");
        }));
}

void LLVM_Hs_disposeSymbolResolver(LLVMSymbolResolverRef resolver) {
    delete resolver;
}

void LLVM_Hs_LinkingLayer_addObject(LinkingLayer *linkLayer, VModuleKey k,
                                    LLVMObjectFileRef objRef,
                                    char **errorMessage) {

    std::unique_ptr<MemoryBuffer> objBuffer =
        unwrap(objRef)->takeBinary().second;
    *errorMessage = nullptr;
    if (auto err = linkLayer->addObject(k, std::move(objBuffer))) {
        std::string error = toString(std::move(err));
        *errorMessage = strdup(error.c_str());
        return;
    }
}

JITTargetAddress LLVM_Hs_JITSymbol_getAddress(LLVMJITSymbolRef symbol,
                                              char **errorMessage) {
    *errorMessage = nullptr;
    if (auto addrOrErr = symbol->getAddress()) {
        // I think this is a bug in LLVM: getAddress() is meant to return '0' for undefined symbols
        // according to https://llvm.org/doxygen/classllvm_1_1JITSymbol.html#a728b38fd41b0dfb04489af84087b8712
        // Reading that more liberally, it should be returning an 'Expect<JITTargetAddress>' whose
        // 'operator bool()' is false (since there is an error)
        // https://llvm.org/doxygen/classllvm_1_1Expected.html#abedc24a1407796eedbee8ba9786d0387
        // However, it clearly is not false since we get in here, and we need to actually
        // attempt to get the value out of the 'Expect<T>' before we finally trigger a failure.
        if (*addrOrErr) {
            return *addrOrErr;
        }
    }
    *errorMessage = strdup("undefined symbol");
    return 0;
}

LLVMJITSymbolFlags LLVM_Hs_JITSymbol_getFlags(LLVMJITSymbolRef symbol) {
    return wrap(symbol->getFlags());
}

const char *LLVM_Hs_JITSymbol_getErrorMsg(LLVMJITSymbolRef symbol) {
    if (!symbol) {
        Error err = symbol->takeError();
        return strdup(toString(std::move(err)).c_str());
    }
    return strdup("");
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

LLVMJITCompileCallbackManagerRef LLVM_Hs_createLocalCompileCallbackManager(
    ExecutionSession *es, const char *triple, JITTargetAddress errorHandler) {
    // We copy the string so that it can be freed on the Haskell side.
    std::string tripleStr(triple);
    auto ccMgr =
        llvm::orc::createLocalCompileCallbackManager(Triple(std::move(tripleStr)), *es, errorHandler);
    if (!ccMgr) {
        std::string errMsg;
        raw_string_ostream errStream(errMsg);
        errStream << ccMgr.takeError();
        reportFatalError(errStream.str());
    }
    return std::move(*ccMgr).release();
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
