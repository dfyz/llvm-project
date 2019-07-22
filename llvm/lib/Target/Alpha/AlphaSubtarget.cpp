//===- AlphaSubtarget.cpp - Alpha Subtarget Information ---------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file implements the Alpha specific subclass of TargetSubtargetInfo.
//
//===----------------------------------------------------------------------===//

#include "AlphaSubtarget.h"
#include "Alpha.h"

#define DEBUG_TYPE "alpha-subtarget"

#define GET_SUBTARGETINFO_TARGET_DESC
#define GET_SUBTARGETINFO_CTOR
#include "AlphaGenSubtargetInfo.inc"

using namespace llvm;

AlphaSubtarget &AlphaSubtarget::initializeSubtargetDependencies(
    const std::string &CPU, const std::string &FS) {
  std::string CPUName = CPU;
  if (CPUName.empty())
    CPUName = "generic";

  // Parse features string.
  ParseSubtargetFeatures(CPUName, FS);

  // Initialize scheduling itinerary for the specified CPU.
  InstrItins = getInstrItineraryForCPU(CPUName);
  return *this;
}

AlphaSubtarget::AlphaSubtarget(const Triple &TT, const std::string &CPU,
                               const std::string &FS, const TargetMachine &TM)
  : AlphaGenSubtargetInfo(TT, CPU, FS), HasCT(false),
    FrameLowering(initializeSubtargetDependencies(CPU, FS)), TLInfo(TM, *this) {}