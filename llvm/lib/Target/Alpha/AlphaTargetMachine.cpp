//===-- AlphaTargetMachine.cpp - Define TargetMachine for Alpha -----------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
//
//===----------------------------------------------------------------------===//

#include "Alpha.h"
#include "AlphaTargetMachine.h"
#include "llvm/CodeGen/TargetLoweringObjectFileImpl.h"
#include "llvm/CodeGen/TargetPassConfig.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/Support/FormattedStream.h"
#include "llvm/Support/TargetRegistry.h"
#include <memory>
using namespace llvm;

extern "C" void LLVMInitializeAlphaTarget() { 
  // Register the target.
  RegisterTargetMachine<AlphaTargetMachine> X(getTheAlphaTarget());
}

AlphaTargetMachine::AlphaTargetMachine(const Target &T, const Triple &TT, StringRef CPU,
                                       StringRef FS, const TargetOptions &Options,
                                       Optional<Reloc::Model> RM, Optional<CodeModel::Model> CM,
                                       CodeGenOpt::Level OL, bool JIT)
  : LLVMTargetMachine(T, "e-m:m-f128:128:128-n64", TT, CPU, FS, Options,
                      Reloc::PIC_, CM.getValueOr(CodeModel::Small), OL),
    TLOF(std::make_unique<TargetLoweringObjectFileELF>()),
    Subtarget(TT, std::string(CPU), std::string(FS), *this)
{
  initAsmInfo();
}

namespace {
class AlphaPassConfig : public TargetPassConfig {
public:
  AlphaPassConfig(AlphaTargetMachine &TM, PassManagerBase &PM)
    : TargetPassConfig(TM, PM)
  {}

  AlphaTargetMachine &getAlphaTargetMachine() const {
    return getTM<AlphaTargetMachine>();
  }

  bool addInstSelector() override;
  void addPreEmitPass() override;
};
}

TargetPassConfig *AlphaTargetMachine::createPassConfig(PassManagerBase &PM) {
  return new AlphaPassConfig(*this, PM);
}

bool AlphaPassConfig::addInstSelector() {
  addPass(createAlphaISelDag(getAlphaTargetMachine()));
  return false;
}
void AlphaPassConfig::addPreEmitPass() {
  // Must run branch selection immediately preceding the asm printer
  addPass(createAlphaBranchSelectionPass());
  addPass(createAlphaLLRPPass(getAlphaTargetMachine()));
}
