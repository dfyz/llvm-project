//===- AlphaRegisterInfo.cpp - Alpha Register Information -------*- C++ -*-===//
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

#define DEBUG_TYPE "reginfo"
#include "Alpha.h"
#include "AlphaFrameLowering.h"
#include "AlphaRegisterInfo.h"
#include "llvm/CodeGen/ValueTypes.h"
#include "llvm/CodeGen/MachineInstrBuilder.h"
#include "llvm/CodeGen/MachineFunction.h"
#include "llvm/CodeGen/MachineFrameInfo.h"
#include "llvm/CodeGen/TargetFrameLowering.h"
#include "llvm/CodeGen/TargetInstrInfo.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Function.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Target/TargetOptions.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/ADT/BitVector.h"
#include "llvm/ADT/STLExtras.h"
#include <cstdlib>

#define GET_REGINFO_TARGET_DESC
#include "AlphaGenRegisterInfo.inc"

using namespace llvm;

AlphaRegisterInfo::AlphaRegisterInfo(const TargetInstrInfo &tii)
  : AlphaGenRegisterInfo(Alpha::R26), TII(tii) {
}

static long getUpper16(long l) {
  long y = l / Alpha::IMM_MULT;
  if (l % Alpha::IMM_MULT > Alpha::IMM_HIGH)
    ++y;
  return y;
}

static long getLower16(long l) {
  long h = getUpper16(l);
  return l - h * Alpha::IMM_MULT;
}

const uint32_t *AlphaRegisterInfo::getCallPreservedMask(const MachineFunction &MF,
                                                        CallingConv::ID) const {
  return CSR_Alpha_RegMask;
}

const MCPhysReg *AlphaRegisterInfo::getCalleeSavedRegs(const MachineFunction *MF)
const {
  return CSR_Alpha_SaveList;
}

BitVector AlphaRegisterInfo::getReservedRegs(const MachineFunction &MF) const {
  BitVector Reserved(getNumRegs());
  Reserved.set(Alpha::R15);
  Reserved.set(Alpha::R29);
  Reserved.set(Alpha::R30);
  Reserved.set(Alpha::R31);
  return Reserved;
}

//Alpha has a slightly funny stack:
//Args
//<- incoming SP
//fixed locals (and spills, callee saved, etc)
//<- FP
//variable locals
//<- SP

void
AlphaRegisterInfo::eliminateFrameIndex(MachineBasicBlock::iterator II,
                                       int SPAdj, unsigned FIOperandNum,
                                       RegScavenger *RS) const {
  assert(SPAdj == 0 && "Unexpected");

  MachineInstr &MI = *II;
  MachineBasicBlock &MBB = *MI.getParent();
  MachineFunction &MF = *MBB.getParent();
  const TargetFrameLowering *TFI = getFrameLowering(MF);

  bool FP = TFI->hasFP(MF);

  int FrameIndex = MI.getOperand(FIOperandNum).getIndex();

  // Add the base register of R30 (SP) or R15 (FP).
  MI.getOperand(FIOperandNum + 1).ChangeToRegister(FP ? Alpha::R15 : Alpha::R30, false);

  // Now add the frame object offset to the offset from the virtual frame index.
  int64_t Offset = MF.getFrameInfo().getObjectOffset(FrameIndex);

  LLVM_DEBUG(errs() << "FI: " << FrameIndex << " Offset: " << Offset << "\n");

  Offset += MF.getFrameInfo().getStackSize();

  LLVM_DEBUG(errs() << "Corrected Offset " << Offset
       << " for stack size: " << MF.getFrameInfo().getStackSize() << "\n");

  if (Offset > Alpha::IMM_HIGH || Offset < Alpha::IMM_LOW) {
    LLVM_DEBUG(errs() << "Unconditionally using R28 for evil purposes Offset: "
          << Offset << "\n");
    //so in this case, we need to use a temporary register, and move the
    //original inst off the SP/FP
    //fix up the old:
    MI.getOperand(FIOperandNum + 1).ChangeToRegister(Alpha::R28, false);
    MI.getOperand(FIOperandNum).ChangeToImmediate(getLower16(Offset));
    //insert the new
    MachineInstr* nMI=BuildMI(MF, MI.getDebugLoc(),
                              TII.get(Alpha::LDAH), Alpha::R28)
      .addImm(getUpper16(Offset)).addReg(FP ? Alpha::R15 : Alpha::R30);
    MBB.insert(II, nMI);
  } else {
    MI.getOperand(FIOperandNum).ChangeToImmediate(Offset);
  }
}

Register AlphaRegisterInfo::getFrameRegister(const MachineFunction &MF) const {
  return getFrameLowering(MF)->hasFP(MF) ? Alpha::R15 : Alpha::R30;
}
