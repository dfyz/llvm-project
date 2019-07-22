//===- AlphaRegisterInfo.h - Alpha Register Information Impl ----*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file contains the Alpha implementation of the TargetRegisterInfo class.
//
//===----------------------------------------------------------------------===//

#ifndef ALPHAREGISTERINFO_H
#define ALPHAREGISTERINFO_H

#include "llvm/CodeGen/TargetRegisterInfo.h"

#define GET_REGINFO_HEADER
#include "AlphaGenRegisterInfo.inc"

namespace llvm {

class TargetInstrInfo;
class Type;

struct AlphaRegisterInfo : public AlphaGenRegisterInfo {
  const TargetInstrInfo &TII;

  AlphaRegisterInfo(const TargetInstrInfo &tii);

  /// Code Generation virtual methods...
  const uint32_t *getCallPreservedMask(const MachineFunction &MF, CallingConv::ID) const override;
  const MCPhysReg *getCalleeSavedRegs(const MachineFunction *MF) const override;

  BitVector getReservedRegs(const MachineFunction &MF) const override;

  void eliminateFrameIndex(MachineBasicBlock::iterator II,
                           int SPAdj, unsigned FIOperandNum,
                           RegScavenger *RS) const override;

  // Debug information queries.
  Register getFrameRegister(const MachineFunction &MF) const override;
};

} // end namespace llvm

#endif
