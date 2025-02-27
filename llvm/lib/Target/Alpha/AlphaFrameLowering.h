//==-- AlphaFrameLowering.h - Define frame lowering for Alpha --*- C++ -*---==//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
//
//
//===----------------------------------------------------------------------===//

#ifndef ALPHA_FRAMEINFO_H
#define ALPHA_FRAMEINFO_H

#include "Alpha.h"
#include "llvm/CodeGen/TargetFrameLowering.h"

namespace llvm {
  class AlphaSubtarget;

class AlphaFrameLowering : public TargetFrameLowering {
  const AlphaSubtarget &STI;
  // FIXME: This should end in MachineFunctionInfo, not here!
  mutable int curgpdist;
public:
  explicit AlphaFrameLowering(const AlphaSubtarget &sti)
    : TargetFrameLowering(StackGrowsDown, Align(16), 0), STI(sti), curgpdist(0) {
  }

  /// emitProlog/emitEpilog - These methods insert prolog and epilog code into
  /// the function.
  void emitPrologue(MachineFunction &MF, MachineBasicBlock &MBB) const override;
  void emitEpilogue(MachineFunction &MF, MachineBasicBlock &MBB) const override;

  bool hasFP(const MachineFunction &MF) const override;

  MachineBasicBlock::iterator eliminateCallFramePseudoInstr(MachineFunction &MF,
                                                            MachineBasicBlock &MBB,
                                                            MachineBasicBlock::iterator I) const override;
};

} // End llvm namespace

#endif
