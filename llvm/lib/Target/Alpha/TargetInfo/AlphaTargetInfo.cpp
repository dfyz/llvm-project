//===-- AlphaTargetInfo.cpp - Alpha Target Implementation -----------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#include "Alpha.h"
#include "llvm/Support/TargetRegistry.h"
using namespace llvm;

Target &getTheAlphaTarget() {
  static Target TheAlphaTarget;
  return TheAlphaTarget;
}

extern "C" void LLVMInitializeAlphaTargetInfo() { 
  RegisterTarget<Triple::alpha, /*HasJIT=*/true>
    X(getTheAlphaTarget(), "alpha", "Alpha [experimental]", "Alpha");
}
