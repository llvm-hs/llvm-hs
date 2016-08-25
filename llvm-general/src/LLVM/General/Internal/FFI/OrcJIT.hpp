#ifndef __LLVM_GENERAL_INTERNAL_FFI__ORC_JIT__HPP__
#define __LLVM_GENERAL_INTERNAL_FFI__ORC_JIT__HPP__

#include "LLVM/General/Internal/FFI/OrcJIT.h"
#include "LLVM/General/Internal/FFI/Target.hpp"

#include "llvm/ExecutionEngine/Orc/IRCompileLayer.h"
#include "llvm/ExecutionEngine/Orc/JITSymbol.h"
#include "llvm/ExecutionEngine/Orc/ObjectLinkingLayer.h"

typedef llvm::orc::ObjectLinkingLayer<> *LLVMObjectLinkingLayerRef;
typedef llvm::orc::IRCompileLayer<llvm::orc::ObjectLinkingLayer<>>
    *LLVMIRCompileLayerRef;
typedef llvm::orc::JITSymbol *LLVMJITSymbolRef;
typedef llvm::orc::ObjectLinkingLayer<>::ObjSetHandleT *LLVMModuleSetHandleRef;

extern "C" {
LLVMIRCompileLayerRef
LLVM_General_createIRCompileLayer(LLVMObjectLinkingLayerRef objectLayer,
                                  LLVMTargetMachineRef tm);

void LLVM_General_disposeIRCompileLayer(LLVMIRCompileLayerRef compileLayer);

LLVMJITSymbolRef LLVM_General_IRCompileLayer_findSymbol(
    LLVMIRCompileLayerRef compileLayer, LLVMTargetDataRef dataLayout,
    const char *name, LLVMBool exportedSymbolsOnly);

void LLVM_General_disposeJITSymbol(LLVMJITSymbolRef symbol);

LLVMModuleSetHandleRef LLVM_General_IRCompileLayer_addModuleSet(
    LLVMIRCompileLayerRef compileLayer, LLVMTargetDataRef dataLayout,
    LLVMModuleRef *modules, unsigned moduleCount,
    void (*dylibResolver)(const char *, LLVMJITSymbolRef),
    void (*externalResolver)(const char *, LLVMJITSymbolRef));

void LLVM_General_IRCompileLayer_removeModuleSet(
    LLVMIRCompileLayerRef compileLayer, LLVMModuleSetHandleRef moduleSetHandle);

LLVMObjectLinkingLayerRef LLVM_General_createObjectLinkingLayer();

void LLVM_General_disposeObjectLinkingLayer(
    LLVMObjectLinkingLayerRef objectLayer);

llvm::orc::TargetAddress
LLVM_General_JITSymbol_getAddress(LLVMJITSymbolRef symbol);

LLVMJITSymbolFlags LLVM_General_JITSymbol_getFlags(LLVMJITSymbolRef symbol);

void LLVM_General_setJITSymbol(LLVMJITSymbolRef symbol,
                               llvm::orc::TargetAddress addr,
                               LLVMJITSymbolFlags flags);

void LLVM_General_getMangledSymbol(char **mangledSymbol, const char *symbol,
                                   LLVMTargetDataRef dataLayout);

void LLVM_General_disposeMangledSymbol(char *mangledSymbol);
}

#endif
