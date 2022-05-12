#define __STDC_LIMIT_MACROS
#include "llvm/Passes/PassBuilder.h"
#include "llvm/Transforms/IPO/GlobalDCE.h"
#include "llvm/Transforms/IPO/AlwaysInliner.h"
#include "llvm/Transforms/IPO/Internalize.h"

using namespace llvm;

namespace {

struct PassBuilderPackage {
  LoopAnalysisManager LAM;
  FunctionAnalysisManager FAM;
  CGSCCAnalysisManager CGAM;
  ModuleAnalysisManager MAM;
  PassBuilder PB;

  PassBuilderPackage(TargetMachine* tm) : PB(tm) {
    PB.registerModuleAnalyses(MAM);
    PB.registerCGSCCAnalyses(CGAM);
    PB.registerFunctionAnalyses(FAM);
    PB.registerLoopAnalyses(LAM);
    PB.crossRegisterProxies(LAM, FAM, CGAM, MAM);
  }
};

}

extern "C" {

PassBuilderPackage* LLVM_Hs_CreatePassBuilderPackage(TargetMachine* tm) {
  return new PassBuilderPackage(tm);
}

void LLVM_Hs_DisposePassBuilderPackage(PassBuilderPackage* pbp) {
  delete pbp;
}

ModulePassManager* LLVM_Hs_CreateModulePassManager() {
  return new ModulePassManager();
}

void LLVM_Hs_DisposeModulePassManager(ModulePassManager* mpm) {
  delete mpm;
}

void LLVM_Hs_ModulePassManagerRun(ModulePassManager *mpm, PassBuilderPackage* pbp, Module* m) {
  mpm->run(*m, pbp->MAM);
}

void LLVM_Hs_AddPerModuleDefaultPipeline(ModulePassManager* mpm, PassBuilderPackage* pbp, int opt) {
  OptimizationLevel opt_lvl;
  if (opt == 0) {
    opt_lvl = OptimizationLevel::O0;
  } if (opt == 1) {
    opt_lvl = OptimizationLevel::O1;
  } else if (opt == 2) {
    opt_lvl = OptimizationLevel::O2;
  } else if (opt >= 3) {
    opt_lvl = OptimizationLevel::O3;
  }
  mpm->addPass(pbp->PB.buildPerModuleDefaultPipeline(opt_lvl));
}

void LLVM_Hs_AddGlobalDeadCodeEliminationPass(ModulePassManager* mpm) {
  mpm->addPass(GlobalDCEPass());
}

void LLVM_Hs_AddAlwaysInlinePass(ModulePassManager* mpm, int insert_lifetimes) {
  mpm->addPass(AlwaysInlinerPass(insert_lifetimes));
}

void LLVM_Hs_AddInternalizeFunctionsPass(ModulePassManager* mpm, int num_exports, char** exports) {
  std::vector<std::string> owned_exports;
  owned_exports.reserve(num_exports);
  for (int i = 0; i < num_exports; ++i) {
    owned_exports.push_back(std::string(exports[i]));
  }
  StringSet<> exports_set;
  for (const std::string& e : owned_exports) exports_set.insert(e);
  mpm->addPass(InternalizePass([owned_exports, exports_set](const GlobalValue &gv) -> bool {
    return exports_set.contains(gv.getName());
  }));
}

}
