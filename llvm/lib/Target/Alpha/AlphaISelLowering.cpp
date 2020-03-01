//===-- AlphaISelLowering.cpp - Alpha DAG Lowering Implementation ---------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file implements the AlphaISelLowering class.
//
//===----------------------------------------------------------------------===//

#include "AlphaISelLowering.h"
#include "AlphaTargetMachine.h"
#include "AlphaMachineFunctionInfo.h"
#include "AlphaSubtarget.h"
#include "llvm/CodeGen/CallingConvLower.h"
#include "llvm/CodeGen/MachineFrameInfo.h"
#include "llvm/CodeGen/MachineFunction.h"
#include "llvm/CodeGen/MachineInstrBuilder.h"
#include "llvm/CodeGen/MachineRegisterInfo.h"
#include "llvm/CodeGen/SelectionDAG.h"
#include "llvm/CodeGen/MachineRegisterInfo.h"
#include "llvm/CodeGen/PseudoSourceValue.h"
#include "llvm/CodeGen/TargetLoweringObjectFileImpl.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Intrinsics.h"
#include "llvm/IR/IntrinsicsAlpha.h"
#include "llvm/IR/Type.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/raw_ostream.h"
using namespace llvm;

/// AddLiveIn - This helper function adds the specified physical register to the
/// MachineFunction as a live in value.  It also creates a corresponding virtual
/// register for it.
static unsigned AddLiveIn(MachineFunction &MF, unsigned PReg,
                          const TargetRegisterClass *RC) {
  assert(RC->contains(PReg) && "Not the correct regclass!");
  unsigned VReg = MF.getRegInfo().createVirtualRegister(RC);
  MF.getRegInfo().addLiveIn(PReg, VReg);
  return VReg;
}

AlphaTargetLowering::AlphaTargetLowering(const TargetMachine &TM,
                                         const AlphaSubtarget &STI)
  : TargetLowering(TM), Subtarget(STI) {
  // Set up the TargetLowering object.
  //I am having problems with shr n i8 1
  setBooleanContents(ZeroOrOneBooleanContent);
  setBooleanVectorContents(ZeroOrOneBooleanContent); // FIXME: Is this correct?

  addRegisterClass(MVT::i64, &Alpha::GPRCRegClass);
  addRegisterClass(MVT::f64, &Alpha::F8RCRegClass);
  addRegisterClass(MVT::f32, &Alpha::F4RCRegClass);

  // We want to custom lower some of our intrinsics.
  setOperationAction(ISD::INTRINSIC_WO_CHAIN, MVT::Other, Custom);

  for (MVT VT : MVT::integer_valuetypes()) {
    setLoadExtAction(ISD::EXTLOAD, VT, MVT::i1,  Promote);
    setLoadExtAction(ISD::ZEXTLOAD, VT, MVT::i1,  Promote);
    setLoadExtAction(ISD::ZEXTLOAD, VT, MVT::i32, Expand);
    setLoadExtAction(ISD::SEXTLOAD, VT, MVT::i1,  Promote);
    setLoadExtAction(ISD::SEXTLOAD, VT, MVT::i8,  Expand);
    setLoadExtAction(ISD::SEXTLOAD, VT, MVT::i16, Expand);
  }

  for (MVT VT : MVT::fp_valuetypes())
    setLoadExtAction(ISD::EXTLOAD, VT, MVT::f32, Expand);

  setTruncStoreAction(MVT::f64, MVT::f32, Expand);

  //  setOperationAction(ISD::BRIND,        MVT::Other,   Expand);
  setOperationAction(ISD::BR_JT, MVT::Other, Expand);
  for (MVT VT : MVT::integer_valuetypes()) {
    setOperationAction(ISD::BR_CC, VT, Expand);
    setOperationAction(ISD::SELECT_CC, VT, Expand);
  }

  setOperationAction(ISD::SIGN_EXTEND_INREG, MVT::i1, Expand);

  setOperationAction(ISD::FREM, MVT::f32, Expand);
  setOperationAction(ISD::FREM, MVT::f64, Expand);

  setOperationAction(ISD::UINT_TO_FP, MVT::i64, Expand);
  setOperationAction(ISD::SINT_TO_FP, MVT::i64, Custom);
  setOperationAction(ISD::FP_TO_UINT, MVT::i64, Expand);
  setOperationAction(ISD::FP_TO_SINT, MVT::i64, Custom);

  if (!STI.hasCT()) {
    setOperationAction(ISD::CTPOP    , MVT::i64  , Expand);
    setOperationAction(ISD::CTTZ     , MVT::i64  , Expand);
    setOperationAction(ISD::CTLZ     , MVT::i64  , Expand);
  }
  setOperationAction(ISD::BSWAP    , MVT::i64, Expand);
  setOperationAction(ISD::ROTL     , MVT::i64, Expand);
  setOperationAction(ISD::ROTR     , MVT::i64, Expand);

  setOperationAction(ISD::SREM     , MVT::i64, Custom);
  setOperationAction(ISD::UREM     , MVT::i64, Custom);
  setOperationAction(ISD::SDIV     , MVT::i64, Custom);
  setOperationAction(ISD::UDIV     , MVT::i64, Custom);

  setOperationAction(ISD::ADDC     , MVT::i64, Expand);
  setOperationAction(ISD::ADDE     , MVT::i64, Expand);
  setOperationAction(ISD::SUBC     , MVT::i64, Expand);
  setOperationAction(ISD::SUBE     , MVT::i64, Expand);

  setOperationAction(ISD::UMUL_LOHI, MVT::i64, Expand);
  setOperationAction(ISD::SMUL_LOHI, MVT::i64, Expand);

  setOperationAction(ISD::SRL_PARTS, MVT::i64, Custom);
  setOperationAction(ISD::SRA_PARTS, MVT::i64, Expand);
  setOperationAction(ISD::SHL_PARTS, MVT::i64, Expand);

  // We don't support sin/cos/sqrt/pow
  setOperationAction(ISD::FSIN , MVT::f64, Expand);
  setOperationAction(ISD::FCOS , MVT::f64, Expand);
  setOperationAction(ISD::FSIN , MVT::f32, Expand);
  setOperationAction(ISD::FCOS , MVT::f32, Expand);

  setOperationAction(ISD::FSQRT, MVT::f64, Expand);
  setOperationAction(ISD::FSQRT, MVT::f32, Expand);

  setOperationAction(ISD::FPOW , MVT::f32, Expand);
  setOperationAction(ISD::FPOW , MVT::f64, Expand);

  setOperationAction(ISD::FMA, MVT::f64, Expand);
  setOperationAction(ISD::FMA, MVT::f32, Expand);

  setOperationAction(ISD::SETCC, MVT::f32, Promote);

  setOperationAction(ISD::BITCAST, MVT::f32, Promote);

  setOperationAction(ISD::EH_LABEL, MVT::Other, Expand);

  // Not implemented yet.
  setOperationAction(ISD::STACKSAVE, MVT::Other, Expand);
  setOperationAction(ISD::STACKRESTORE, MVT::Other, Expand);
  setOperationAction(ISD::DYNAMIC_STACKALLOC, MVT::i64, Expand);

  // We want to legalize GlobalAddress and ConstantPool and
  // ExternalSymbols nodes into the appropriate instructions to
  // materialize the address.
  setOperationAction(ISD::GlobalAddress,  MVT::i64, Custom);
  setOperationAction(ISD::ConstantPool,   MVT::i64, Custom);
  setOperationAction(ISD::ExternalSymbol, MVT::i64, Custom);
  setOperationAction(ISD::GlobalTLSAddress, MVT::i64, Custom);

  setOperationAction(ISD::VASTART, MVT::Other, Custom);
  setOperationAction(ISD::VAEND,   MVT::Other, Expand);
  setOperationAction(ISD::VACOPY,  MVT::Other, Custom);
  setOperationAction(ISD::VAARG,   MVT::Other, Custom);
  setOperationAction(ISD::VAARG,   MVT::i32,   Custom);

  setOperationAction(ISD::JumpTable, MVT::i64, Custom);
  setOperationAction(ISD::JumpTable, MVT::i32, Custom);

  setOperationAction(ISD::ATOMIC_LOAD,  MVT::i32, Expand);
  setOperationAction(ISD::ATOMIC_STORE, MVT::i32, Expand);

  setOperationAction(ISD::READCYCLECOUNTER, MVT::i64, Legal);

  setStackPointerRegisterToSaveRestore(Alpha::R30);

  setMinFunctionAlignment(Align(16));

  computeRegisterProperties(STI.getRegisterInfo());
}

EVT AlphaTargetLowering::getSetCCResultType(const DataLayout &, LLVMContext &, EVT VT) const {
  return MVT::i64;
}

const char *AlphaTargetLowering::getTargetNodeName(unsigned Opcode) const {
  switch (Opcode) {
  default: return 0;
  case AlphaISD::CVTQT_: return "Alpha::CVTQT_";
  case AlphaISD::CVTQS_: return "Alpha::CVTQS_";
  case AlphaISD::CVTTQ_: return "Alpha::CVTTQ_";
  case AlphaISD::GPRelHi: return "Alpha::GPRelHi";
  case AlphaISD::GPRelLo: return "Alpha::GPRelLo";
  case AlphaISD::RelLit: return "Alpha::RelLit";
  case AlphaISD::GlobalRetAddr: return "Alpha::GlobalRetAddr";
  case AlphaISD::CALL:   return "Alpha::CALL";
  case AlphaISD::DivCall: return "Alpha::DivCall";
  case AlphaISD::RET_FLAG: return "Alpha::RET_FLAG";
  case AlphaISD::COND_BRANCH_I: return "Alpha::COND_BRANCH_I";
  case AlphaISD::COND_BRANCH_F: return "Alpha::COND_BRANCH_F";
  }
}

static SDValue LowerJumpTable(SDValue Op, SelectionDAG &DAG) {
  EVT PtrVT = Op.getValueType();
  JumpTableSDNode *JT = cast<JumpTableSDNode>(Op);
  SDValue JTI = DAG.getTargetJumpTable(JT->getIndex(), PtrVT);
  // FIXME there isn't really any debug info here
  SDLoc dl(Op);

  SDValue Hi = DAG.getNode(AlphaISD::GPRelHi,  dl, MVT::i64, JTI,
                             DAG.getGLOBAL_OFFSET_TABLE(MVT::i64));
  SDValue Lo = DAG.getNode(AlphaISD::GPRelLo, dl, MVT::i64, JTI, Hi);
  return Lo;
}

//http://www.cs.arizona.edu/computer.help/policy/DIGITAL_unix/
//AA-PY8AC-TET1_html/callCH3.html#BLOCK21

//For now, just use variable size stack frame format

//In a standard call, the first six items are passed in registers $16
//- $21 and/or registers $f16 - $f21. (See Section 4.1.2 for details
//of argument-to-register correspondence.) The remaining items are
//collected in a memory argument list that is a naturally aligned
//array of quadwords. In a standard call, this list, if present, must
//be passed at 0(SP).
//7 ... n         0(SP) ... (n-7)*8(SP)

// //#define FP    $15
// //#define RA    $26
// //#define PV    $27
// //#define GP    $29
// //#define SP    $30

#include "AlphaGenCallingConv.inc"

SDValue
AlphaTargetLowering::LowerCall(CallLoweringInfo &CLI,
                               SmallVectorImpl<SDValue> &InVals) const {
  // Alpha target does not yet support tail call optimization.
  CLI.IsTailCall = false;

  // Analyze operands of the call, assigning locations to each operand.
  SmallVector<CCValAssign, 16> ArgLocs;
  CCState CCInfo(CLI.CallConv, CLI.IsVarArg, CLI.DAG.getMachineFunction(),
		 ArgLocs, *CLI.DAG.getContext());

  CCInfo.AnalyzeCallOperands(CLI.Outs, CC_Alpha);

    // Get a count of how many bytes are to be pushed on the stack.
  unsigned NumBytes = CCInfo.getNextStackOffset();

  CLI.Chain = CLI.DAG.getCALLSEQ_START(CLI.Chain, NumBytes, 0, CLI.DL);

  SmallVector<std::pair<unsigned, SDValue>, 4> RegsToPass;
  SmallVector<SDValue, 12> MemOpChains;
  SDValue StackPtr;

  // Walk the register/memloc assignments, inserting copies/loads.
  for (size_t i = 0, e = ArgLocs.size(); i != e; ++i) {
    CCValAssign &VA = ArgLocs[i];

    SDValue Arg = CLI.OutVals[i];

    // Promote the value if needed.
    switch (VA.getLocInfo()) {
      default: assert(0 && "Unknown loc info!");
      case CCValAssign::Full: break;
      case CCValAssign::SExt:
        Arg = CLI.DAG.getNode(ISD::SIGN_EXTEND, CLI.DL, VA.getLocVT(), Arg);
        break;
      case CCValAssign::ZExt:
        Arg = CLI.DAG.getNode(ISD::ZERO_EXTEND, CLI.DL, VA.getLocVT(), Arg);
        break;
      case CCValAssign::AExt:
        Arg = CLI.DAG.getNode(ISD::ANY_EXTEND, CLI.DL, VA.getLocVT(), Arg);
        break;
    }

    // Arguments that can be passed on register must be kept at RegsToPass
    // vector
    if (VA.isRegLoc()) {
      RegsToPass.push_back(std::make_pair(VA.getLocReg(), Arg));
    } else {
      assert(VA.isMemLoc());

      if (StackPtr.getNode() == 0)
        StackPtr = CLI.DAG.getCopyFromReg(CLI.Chain, CLI.DL, Alpha::R30, MVT::i64);

      SDValue PtrOff = CLI.DAG.getNode(ISD::ADD, CLI.DL, getPointerTy(CLI.DAG.getDataLayout()),
                                   StackPtr,
                                   CLI.DAG.getIntPtrConstant(VA.getLocMemOffset(), CLI.DL));

      MemOpChains.push_back(CLI.DAG.getStore(CLI.Chain, CLI.DL, Arg, PtrOff, MachinePointerInfo()));
    }
  }

  // Transform all store nodes into one single node because all store nodes are
  // independent of each other.
  if (!MemOpChains.empty())
    CLI.Chain = CLI.DAG.getNode(ISD::TokenFactor, CLI.DL, MVT::Other, MemOpChains);

  // Build a sequence of copy-to-reg nodes chained together with token chain and
  // flag operands which copy the outgoing args into registers.  The InFlag in
  // necessary since all emitted instructions must be stuck together.
  SDValue InFlag;
  for (unsigned i = 0, e = RegsToPass.size(); i != e; ++i) {
    CLI.Chain = CLI.DAG.getCopyToReg(CLI.Chain, CLI.DL, RegsToPass[i].first,
                             RegsToPass[i].second, InFlag);
    InFlag = CLI.Chain.getValue(1);
  }

  // Returns a chain & a flag for retval copy to use.
  SDVTList NodeTys = CLI.DAG.getVTList(MVT::Other, MVT::Glue);
  SmallVector<SDValue, 8> Ops;
  Ops.push_back(CLI.Chain);
  Ops.push_back(CLI.Callee);

  // Add argument registers to the end of the list so that they are
  // known live into the call.
  for (unsigned i = 0, e = RegsToPass.size(); i != e; ++i)
    Ops.push_back(CLI.DAG.getRegister(RegsToPass[i].first,
                                  RegsToPass[i].second.getValueType()));

  const uint32_t *Mask =
      Subtarget.getRegisterInfo()->getCallPreservedMask(CLI.DAG.getMachineFunction(), CLI.CallConv);
  Ops.push_back(CLI.DAG.getRegisterMask(Mask));

  if (InFlag.getNode())
    Ops.push_back(InFlag);

  CLI.Chain = CLI.DAG.getNode(AlphaISD::CALL, CLI.DL, NodeTys, Ops);
  InFlag = CLI.Chain.getValue(1);

  auto PointerTy = getPointerTy(CLI.DAG.getDataLayout());
  // Create the CALLSEQ_END node.
  CLI.Chain = CLI.DAG.getCALLSEQ_END(CLI.Chain,
                             CLI.DAG.getConstant(NumBytes, CLI.DL, PointerTy, true),
                             CLI.DAG.getConstant(0, CLI.DL, PointerTy, true),
                             InFlag,
                             CLI.DL);
  InFlag = CLI.Chain.getValue(1);

  // Handle result values, copying them out of physregs into vregs that we
  // return.
  return LowerCallResult(CLI.Chain, InFlag, CLI.CallConv, CLI.IsVarArg,
                         CLI.Ins, CLI.DL, CLI.DAG, InVals);
}

bool AlphaTargetLowering::CanLowerReturn(CallingConv::ID CallConv,
                    MachineFunction &MF,
                    bool isVarArg,
                    const SmallVectorImpl<ISD::OutputArg> &Outs,
                    LLVMContext &Context) const {
  return Outs.size() <= 1;
}

/// LowerCallResult - Lower the result values of a call into the
/// appropriate copies out of appropriate physical registers.
///
SDValue
AlphaTargetLowering::LowerCallResult(SDValue Chain, SDValue InFlag,
                                     CallingConv::ID CallConv, bool isVarArg,
                                     const SmallVectorImpl<ISD::InputArg> &Ins,
                                     const SDLoc &dl, SelectionDAG &DAG,
                                     SmallVectorImpl<SDValue> &InVals) const {

  // Assign locations to each value returned by this call.
  SmallVector<CCValAssign, 16> RVLocs;
  CCState CCInfo(CallConv, isVarArg, DAG.getMachineFunction(), RVLocs, *DAG.getContext());

  CCInfo.AnalyzeCallResult(Ins, RetCC_Alpha);

  // Copy all of the result registers out of their specified physreg.
  for (unsigned i = 0; i != RVLocs.size(); ++i) {
    CCValAssign &VA = RVLocs[i];

    Chain = DAG.getCopyFromReg(Chain, dl, VA.getLocReg(),
                               VA.getLocVT(), InFlag).getValue(1);
    SDValue RetValue = Chain.getValue(0);
    InFlag = Chain.getValue(2);

    // If this is an 8/16/32-bit value, it is really passed promoted to 64
    // bits. Insert an assert[sz]ext to capture this, then truncate to the
    // right size.
    if (VA.getLocInfo() == CCValAssign::SExt)
      RetValue = DAG.getNode(ISD::AssertSext, dl, VA.getLocVT(), RetValue,
                             DAG.getValueType(VA.getValVT()));
    else if (VA.getLocInfo() == CCValAssign::ZExt)
      RetValue = DAG.getNode(ISD::AssertZext, dl, VA.getLocVT(), RetValue,
                             DAG.getValueType(VA.getValVT()));

    if (VA.getLocInfo() != CCValAssign::Full)
      RetValue = DAG.getNode(ISD::TRUNCATE, dl, VA.getValVT(), RetValue);

    InVals.push_back(RetValue);
  }

  return Chain;
}

SDValue
AlphaTargetLowering::LowerFormalArguments(SDValue Chain,
                                          CallingConv::ID CallConv, bool isVarArg,
                                          const SmallVectorImpl<ISD::InputArg> &Ins,
                                          const SDLoc &dl, SelectionDAG &DAG,
                                          SmallVectorImpl<SDValue> &InVals) const {
  MachineFunction &MF = DAG.getMachineFunction();
  MachineFrameInfo &MFI = MF.getFrameInfo();
  AlphaMachineFunctionInfo *FuncInfo = MF.getInfo<AlphaMachineFunctionInfo>();

  unsigned args_int[] = {
    Alpha::R16, Alpha::R17, Alpha::R18, Alpha::R19, Alpha::R20, Alpha::R21};
  unsigned args_float[] = {
    Alpha::F16, Alpha::F17, Alpha::F18, Alpha::F19, Alpha::F20, Alpha::F21};

  for (size_t ArgNo = 0, e = Ins.size(); ArgNo != e; ++ArgNo) {
    SDValue argt;
    EVT ObjectVT = Ins[ArgNo].VT;
    SDValue ArgVal;

    if (ArgNo  < 6) {
      switch (ObjectVT.getSimpleVT().SimpleTy) {
      default:
        assert(false && "Invalid value type!");
      case MVT::f64:
        args_float[ArgNo] = AddLiveIn(MF, args_float[ArgNo],
                                      &Alpha::F8RCRegClass);
        ArgVal = DAG.getCopyFromReg(Chain, dl, args_float[ArgNo], ObjectVT);
        break;
      case MVT::f32:
        args_float[ArgNo] = AddLiveIn(MF, args_float[ArgNo],
                                      &Alpha::F4RCRegClass);
        ArgVal = DAG.getCopyFromReg(Chain, dl, args_float[ArgNo], ObjectVT);
        break;
      case MVT::i64:
        args_int[ArgNo] = AddLiveIn(MF, args_int[ArgNo],
                                    &Alpha::GPRCRegClass);
        ArgVal = DAG.getCopyFromReg(Chain, dl, args_int[ArgNo], MVT::i64);
        break;
      }
    } else { //more args
      // Create the frame index object for this incoming parameter...
      int FI = MFI.CreateFixedObject(8, 8 * (ArgNo - 6), true);

      // Create the SelectionDAG nodes corresponding to a load
      //from this parameter
      SDValue FIN = DAG.getFrameIndex(FI, MVT::i64);
      ArgVal = DAG.getLoad(ObjectVT, dl, Chain, FIN, MachinePointerInfo());
    }
    InVals.push_back(ArgVal);
  }

  // If the functions takes variable number of arguments, copy all regs to stack
  if (isVarArg) {
    FuncInfo->setVarArgsOffset(Ins.size() * 8);
    std::vector<SDValue> LS;
    for (int i = 0; i < 6; ++i) {
      if (Register::isPhysicalRegister(args_int[i]))
        args_int[i] = AddLiveIn(MF, args_int[i], &Alpha::GPRCRegClass);
      SDValue argt = DAG.getCopyFromReg(Chain, dl, args_int[i], MVT::i64);
      int FI = MFI.CreateFixedObject(8, -8 * (6 - i), true);
      if (i == 0) FuncInfo->setVarArgsBase(FI);
      SDValue SDFI = DAG.getFrameIndex(FI, MVT::i64);
      LS.push_back(DAG.getStore(Chain, dl, argt, SDFI, MachinePointerInfo()));

      if (Register::isPhysicalRegister(args_float[i]))
        args_float[i] = AddLiveIn(MF, args_float[i], &Alpha::F8RCRegClass);
      argt = DAG.getCopyFromReg(Chain, dl, args_float[i], MVT::f64);
      FI = MFI.CreateFixedObject(8, - 8 * (12 - i), true);
      SDFI = DAG.getFrameIndex(FI, MVT::i64);
      LS.push_back(DAG.getStore(Chain, dl, argt, SDFI, MachinePointerInfo()));
    }

    //Set up a token factor with all the stack traffic
    Chain = DAG.getNode(ISD::TokenFactor, dl, MVT::Other, LS);
  }

  return Chain;
}

SDValue
AlphaTargetLowering::LowerReturn(SDValue Chain,
                                 CallingConv::ID CallConv, bool isVarArg,
                                 const SmallVectorImpl<ISD::OutputArg> &Outs,
                                 const SmallVectorImpl<SDValue> &OutVals,
                                 const SDLoc &dl, SelectionDAG &DAG) const {

  SDValue Copy = DAG.getCopyToReg(Chain, dl, Alpha::R26,
                                  DAG.getNode(AlphaISD::GlobalRetAddr, SDLoc(), MVT::i64),
                                  SDValue());
  SmallVector<SDValue, 3> RetOps(1, Copy);
  switch (Outs.size()) {
  default:
    llvm_unreachable("Do not know how to return this many arguments!");
  case 0:
    break;
  case 1: {
    EVT ArgVT = Outs[0].VT;
    unsigned ArgReg;
    if (ArgVT.isInteger())
      ArgReg = Alpha::R0;
    else {
      assert(ArgVT.isFloatingPoint());
      ArgReg = Alpha::F0;
    }
    Copy = DAG.getCopyToReg(Copy, dl, ArgReg, OutVals[0], Copy.getValue(1));
    RetOps.front() = Copy;
    RetOps.push_back(DAG.getRegister(ArgReg, ArgVT));
    break;
  }
  }
  RetOps.push_back(Copy.getValue(1));
  return DAG.getNode(AlphaISD::RET_FLAG, dl, MVT::Other, RetOps);
}

void AlphaTargetLowering::LowerVAARG(SDNode *N, SDValue &Chain,
                                     SDValue &DataPtr,
                                     SelectionDAG &DAG) const {
  Chain = N->getOperand(0);
  SDValue VAListP = N->getOperand(1);
  const Value *VAListS = cast<SrcValueSDNode>(N->getOperand(2))->getValue();
  SDLoc dl(N);

  SDValue Base = DAG.getLoad(MVT::i64, dl, Chain, VAListP,
                             MachinePointerInfo(VAListS));
  SDValue Tmp = DAG.getNode(ISD::ADD, dl, MVT::i64, VAListP,
                              DAG.getConstant(8, dl, MVT::i64));
  SDValue Offset = DAG.getExtLoad(ISD::SEXTLOAD, dl, MVT::i64, Base.getValue(1),
                                  Tmp, MachinePointerInfo(),
                                  MVT::i32);
  DataPtr = DAG.getNode(ISD::ADD, dl, MVT::i64, Base, Offset);
  if (N->getValueType(0).isFloatingPoint())
  {
    //if fp && Offset < 6*8, then subtract 6*8 from DataPtr
    SDValue FPDataPtr = DAG.getNode(ISD::SUB, dl, MVT::i64, DataPtr,
                                      DAG.getConstant(8*6, dl, MVT::i64));
    SDValue CC = DAG.getSetCC(dl, MVT::i64, Offset,
                                DAG.getConstant(8*6, dl, MVT::i64), ISD::SETLT);
    DataPtr = DAG.getNode(ISD::SELECT, dl, MVT::i64, CC, FPDataPtr, DataPtr);
  }

  SDValue NewOffset = DAG.getNode(ISD::ADD, dl, MVT::i64, Offset,
                                    DAG.getConstant(8, dl, MVT::i64));
  Chain = DAG.getTruncStore(Offset.getValue(1), dl, NewOffset, Tmp,
                            MachinePointerInfo(),
                            MVT::i32);
}

/// LowerOperation - Provide custom lowering hooks for some operations.
///
SDValue AlphaTargetLowering::LowerOperation(SDValue Op,
                                            SelectionDAG &DAG) const {
  SDLoc dl(Op);
  switch (Op.getOpcode()) {
  default: llvm_unreachable("Wasn't expecting to be able to lower this!");
  case ISD::JumpTable: return LowerJumpTable(Op, DAG);

  case ISD::INTRINSIC_WO_CHAIN: {
    unsigned IntNo = cast<ConstantSDNode>(Op.getOperand(0))->getZExtValue();
    switch (IntNo) {
    default: break;    // Don't custom lower most intrinsics.
    case Intrinsic::alpha_umulh:
      return DAG.getNode(ISD::MULHU, dl, MVT::i64,
                         Op.getOperand(1), Op.getOperand(2));
    }
    break;
  }

  case ISD::SRL_PARTS: {
    SDValue ShOpLo = Op.getOperand(0);
    SDValue ShOpHi = Op.getOperand(1);
    SDValue ShAmt  = Op.getOperand(2);
    SDValue bm = DAG.getNode(ISD::SUB, dl, MVT::i64,
                             DAG.getConstant(64, dl, MVT::i64), ShAmt);
    SDValue BMCC = DAG.getSetCC(dl, MVT::i64, bm,
                                DAG.getConstant(0, dl, MVT::i64), ISD::SETLE);
    // if 64 - shAmt <= 0
    SDValue Hi_Neg = DAG.getConstant(0, dl, MVT::i64);
    SDValue ShAmt_Neg = DAG.getNode(ISD::SUB, dl, MVT::i64,
                                    DAG.getConstant(0, dl, MVT::i64), bm);
    SDValue Lo_Neg = DAG.getNode(ISD::SRL, dl, MVT::i64, ShOpHi, ShAmt_Neg);
    // else
    SDValue carries = DAG.getNode(ISD::SHL, dl, MVT::i64, ShOpHi, bm);
    SDValue Hi_Pos =  DAG.getNode(ISD::SRL, dl, MVT::i64, ShOpHi, ShAmt);
    SDValue Lo_Pos = DAG.getNode(ISD::SRL, dl, MVT::i64, ShOpLo, ShAmt);
    Lo_Pos = DAG.getNode(ISD::OR, dl, MVT::i64, Lo_Pos, carries);
    // Merge
    SDValue Hi = DAG.getNode(ISD::SELECT, dl, MVT::i64, BMCC, Hi_Neg, Hi_Pos);
    SDValue Lo = DAG.getNode(ISD::SELECT, dl, MVT::i64, BMCC, Lo_Neg, Lo_Pos);
    SDValue Ops[2] = { Lo, Hi };
    return DAG.getMergeValues(Ops, dl);
  }
    //  case ISD::SRA_PARTS:

    //  case ISD::SHL_PARTS:


  case ISD::SINT_TO_FP: {
    assert(Op.getOperand(0).getValueType() == MVT::i64 &&
           "Unhandled SINT_TO_FP type in custom expander!");
    SDValue LD;
    bool isDouble = Op.getValueType() == MVT::f64;
    LD = DAG.getNode(ISD::BITCAST, dl, MVT::f64, Op.getOperand(0));
    SDValue FP = DAG.getNode(isDouble?AlphaISD::CVTQT_:AlphaISD::CVTQS_, dl,
                               isDouble?MVT::f64:MVT::f32, LD);
    return FP;
  }
  case ISD::FP_TO_SINT: {
    bool isDouble = Op.getOperand(0).getValueType() == MVT::f64;
    SDValue src = Op.getOperand(0);

    if (!isDouble) //Promote
      src = DAG.getNode(ISD::FP_EXTEND, dl, MVT::f64, src);

    src = DAG.getNode(AlphaISD::CVTTQ_, dl, MVT::f64, src);

    return DAG.getNode(ISD::BITCAST, dl, MVT::i64, src);
  }
  case ISD::ConstantPool: {
    ConstantPoolSDNode *CP = cast<ConstantPoolSDNode>(Op);
    const Constant *C = CP->getConstVal();
    SDValue CPI = DAG.getTargetConstantPool(C, MVT::i64, CP->getAlignment());
    // FIXME there isn't really any debug info here

    SDValue Hi = DAG.getNode(AlphaISD::GPRelHi,  dl, MVT::i64, CPI,
                               DAG.getGLOBAL_OFFSET_TABLE(MVT::i64));
    SDValue Lo = DAG.getNode(AlphaISD::GPRelLo, dl, MVT::i64, CPI, Hi);
    return Lo;
  }
  case ISD::GlobalTLSAddress:
    llvm_unreachable("TLS not implemented for Alpha.");
  case ISD::GlobalAddress: {
    GlobalAddressSDNode *GSDN = cast<GlobalAddressSDNode>(Op);
    const GlobalValue *GV = GSDN->getGlobal();
    SDValue GA = DAG.getTargetGlobalAddress(GV, dl, MVT::i64,
                                            GSDN->getOffset());
    // FIXME there isn't really any debug info here

    //    if (!GV->hasWeakLinkage() && !GV->isDeclaration()
    //        && !GV->hasLinkOnceLinkage()) {
    if (GV->hasLocalLinkage()) {
      SDValue Hi = DAG.getNode(AlphaISD::GPRelHi,  dl, MVT::i64, GA,
                                DAG.getGLOBAL_OFFSET_TABLE(MVT::i64));
      SDValue Lo = DAG.getNode(AlphaISD::GPRelLo, dl, MVT::i64, GA, Hi);
      return Lo;
    } else
      return DAG.getNode(AlphaISD::RelLit, dl, MVT::i64, GA,
                         DAG.getGLOBAL_OFFSET_TABLE(MVT::i64));
  }
  case ISD::ExternalSymbol: {
    return DAG.getNode(AlphaISD::RelLit, dl, MVT::i64,
                       DAG.getTargetExternalSymbol(cast<ExternalSymbolSDNode>(Op)
                                                   ->getSymbol(), MVT::i64),
                       DAG.getGLOBAL_OFFSET_TABLE(MVT::i64));
  }

  case ISD::UREM:
  case ISD::SREM:
    //Expand only on constant case
    if (Op.getOperand(1).getOpcode() == ISD::Constant) {
      EVT VT = Op.getNode()->getValueType(0);
      SmallVector<SDNode *, 16> Dummy;
      SDValue Tmp1 = Op.getNode()->getOpcode() == ISD::UREM ?
        BuildUDIV(Op.getNode(), DAG, false, Dummy) :
        BuildSDIV(Op.getNode(), DAG, false, Dummy);
      Tmp1 = DAG.getNode(ISD::MUL, dl, VT, Tmp1, Op.getOperand(1));
      Tmp1 = DAG.getNode(ISD::SUB, dl, VT, Op.getOperand(0), Tmp1);
      return Tmp1;
    }
    //fall through
  case ISD::SDIV:
  case ISD::UDIV:
    if (Op.getValueType().isInteger()) {
      if (Op.getOperand(1).getOpcode() == ISD::Constant) {
        SmallVector<SDNode *, 16> Dummy;
        return Op.getOpcode() == ISD::SDIV
          ? BuildSDIV(Op.getNode(), DAG, false, Dummy)
          : BuildUDIV(Op.getNode(), DAG, false, Dummy);
      }
      const char* opstr = 0;
      switch (Op.getOpcode()) {
      case ISD::UREM: opstr = "__remqu"; break;
      case ISD::SREM: opstr = "__remq";  break;
      case ISD::UDIV: opstr = "__divqu"; break;
      case ISD::SDIV: opstr = "__divq";  break;
      }
      SDValue Tmp1 = Op.getOperand(0),
        Tmp2 = Op.getOperand(1),
        Addr = DAG.getExternalSymbol(opstr, MVT::i64);
      return DAG.getNode(AlphaISD::DivCall, dl, MVT::i64, Addr, Tmp1, Tmp2);
    }
    break;

  case ISD::VAARG: {
    SDValue Chain, DataPtr;
    LowerVAARG(Op.getNode(), Chain, DataPtr, DAG);

    SDValue Result;
    if (Op.getValueType() == MVT::i32)
      Result = DAG.getExtLoad(ISD::SEXTLOAD, dl, MVT::i64, Chain, DataPtr,
                              MachinePointerInfo(), MVT::i32);
    else
      Result = DAG.getLoad(Op.getValueType(), dl, Chain, DataPtr,
                           MachinePointerInfo());
    return Result;
  }
  case ISD::VACOPY: {
    SDValue Chain = Op.getOperand(0);
    SDValue DestP = Op.getOperand(1);
    SDValue SrcP = Op.getOperand(2);
    const Value *DestS = cast<SrcValueSDNode>(Op.getOperand(3))->getValue();
    const Value *SrcS = cast<SrcValueSDNode>(Op.getOperand(4))->getValue();

    SDValue Val = DAG.getLoad(getPointerTy(DAG.getDataLayout()), dl, Chain, SrcP,
                              MachinePointerInfo(SrcS));
    SDValue Result = DAG.getStore(Val.getValue(1), dl, Val, DestP, MachinePointerInfo(DestS));
    SDValue NP = DAG.getNode(ISD::ADD, dl, MVT::i64, SrcP,
                               DAG.getConstant(8, dl, MVT::i64));
    Val = DAG.getExtLoad(ISD::SEXTLOAD, dl, MVT::i64, Result,
                         NP, MachinePointerInfo(), MVT::i32);
    SDValue NPD = DAG.getNode(ISD::ADD, dl, MVT::i64, DestP,
                                DAG.getConstant(8, dl, MVT::i64));
    return DAG.getTruncStore(Val.getValue(1), dl, Val, NPD,
                             MachinePointerInfo(), MVT::i32);
  }
  case ISD::VASTART: {
    MachineFunction &MF = DAG.getMachineFunction();
    AlphaMachineFunctionInfo *FuncInfo = MF.getInfo<AlphaMachineFunctionInfo>();

    SDValue Chain = Op.getOperand(0);
    SDValue VAListP = Op.getOperand(1);
    const Value *VAListS = cast<SrcValueSDNode>(Op.getOperand(2))->getValue();

    // vastart stores the address of the VarArgsBase and VarArgsOffset
    SDValue FR  = DAG.getFrameIndex(FuncInfo->getVarArgsBase(), MVT::i64);
    SDValue S1  = DAG.getStore(Chain, dl, FR, VAListP, MachinePointerInfo(VAListS));
    SDValue SA2 = DAG.getNode(ISD::ADD, dl, MVT::i64, VAListP,
                                DAG.getConstant(8, dl, MVT::i64));
    return DAG.getTruncStore(S1, dl,
                             DAG.getConstant(FuncInfo->getVarArgsOffset(), dl,
                                             MVT::i64),
                             SA2, MachinePointerInfo(),
                             MVT::i32);
  }
  case ISD::RETURNADDR:
    return DAG.getNode(AlphaISD::GlobalRetAddr, SDLoc(), MVT::i64);
      //FIXME: implement
  case ISD::FRAMEADDR:          break;
  }

  return SDValue();
}

void AlphaTargetLowering::ReplaceNodeResults(SDNode *N,
                                             SmallVectorImpl<SDValue>&Results,
                                             SelectionDAG &DAG) const {
  SDLoc dl(N);
  assert(N->getValueType(0) == MVT::i32 &&
         N->getOpcode() == ISD::VAARG &&
         "Unknown node to custom promote!");

  SDValue Chain, DataPtr;
  LowerVAARG(N, Chain, DataPtr, DAG);
  SDValue Res = DAG.getLoad(N->getValueType(0), dl, Chain, DataPtr,
                            MachinePointerInfo());
  Results.push_back(Res);
  Results.push_back(SDValue(Res.getNode(), 1));
}


//Inline Asm

/// getConstraintType - Given a constraint letter, return the type of
/// constraint it is for this target.
AlphaTargetLowering::ConstraintType
AlphaTargetLowering::getConstraintType(StringRef  Constraint) const {
  if (Constraint.size() == 1) {
    switch (Constraint[0]) {
    default: break;
    case 'f':
    case 'r':
      return C_RegisterClass;
    }
  }
  return TargetLowering::getConstraintType(Constraint);
}

/// Examine constraint type and operand type and determine a weight value.
/// This object must already have been set up with the operand type
/// and the current alternative constraint selected.
TargetLowering::ConstraintWeight
AlphaTargetLowering::getSingleConstraintMatchWeight(
    AsmOperandInfo &info, const char *constraint) const {
  ConstraintWeight weight = CW_Invalid;
  Value *CallOperandVal = info.CallOperandVal;
    // If we don't have a value, we can't do a match,
    // but allow it at the lowest weight.
  if (CallOperandVal == nullptr)
    return CW_Default;
  // Look at the constraint type.
  switch (*constraint) {
  default:
    weight = TargetLowering::getSingleConstraintMatchWeight(info, constraint);
    break;
  case 'f':
    weight = CW_Register;
    break;
  }
  return weight;
}

/// Given a register class constraint, like 'r', if this corresponds directly
/// to an LLVM register class, return a register of 0 and the register class
/// pointer.
std::pair<unsigned, const TargetRegisterClass*>
AlphaTargetLowering::getRegForInlineAsmConstraint(const TargetRegisterInfo *TRI,
                                                  StringRef Constraint, MVT VT) const
{
  if (Constraint.size() == 1) {
    switch (Constraint[0]) {
    case 'r':
      return std::make_pair(0U, &Alpha::GPRCRegClass);
    case 'f':
      return VT == MVT::f64 ? std::make_pair(0U, &Alpha::F8RCRegClass) :
	std::make_pair(0U, &Alpha::F4RCRegClass);
    }
  }
  return TargetLowering::getRegForInlineAsmConstraint(TRI, Constraint, VT);
}

//===----------------------------------------------------------------------===//
//  Other Lowering Code
//===----------------------------------------------------------------------===//

MachineBasicBlock *
AlphaTargetLowering::EmitInstrWithCustomInserter(MachineInstr &MI,
                                                 MachineBasicBlock *BB) const {
  const TargetInstrInfo *TII = Subtarget.getInstrInfo();
  assert((MI.getOpcode() == Alpha::CAS32 ||
          MI.getOpcode() == Alpha::CAS64 ||
          MI.getOpcode() == Alpha::LAS32 ||
          MI.getOpcode() == Alpha::LAS64 ||
          MI.getOpcode() == Alpha::SWAP32 ||
          MI.getOpcode() == Alpha::SWAP64) &&
         "Unexpected instr type to insert");

  bool is32 = MI.getOpcode() == Alpha::CAS32 ||
    MI.getOpcode() == Alpha::LAS32 ||
    MI.getOpcode() == Alpha::SWAP32;

  //Load locked store conditional for atomic ops take on the same form
  //start:
  //ll
  //do stuff (maybe branch to exit)
  //sc
  //test sc and maybe branck to start
  //exit:
  const BasicBlock *LLVM_BB = BB->getBasicBlock();
  DebugLoc dl = MI.getDebugLoc();
  MachineFunction::iterator It = BB->getIterator();
  ++It;

  MachineBasicBlock *thisMBB = BB;
  MachineFunction *F = BB->getParent();
  MachineBasicBlock *llscMBB = F->CreateMachineBasicBlock(LLVM_BB);
  MachineBasicBlock *sinkMBB = F->CreateMachineBasicBlock(LLVM_BB);

  sinkMBB->splice(sinkMBB->begin(), thisMBB,
                  std::next(MachineBasicBlock::iterator(MI)),
                  thisMBB->end());
  sinkMBB->transferSuccessorsAndUpdatePHIs(thisMBB);

  F->insert(It, llscMBB);
  F->insert(It, sinkMBB);

  BuildMI(thisMBB, dl, TII->get(Alpha::BR)).addMBB(llscMBB);

  unsigned reg_res = MI.getOperand(0).getReg(),
    reg_ptr = MI.getOperand(1).getReg(),
    reg_v2 = MI.getOperand(2).getReg(),
    reg_store = F->getRegInfo().createVirtualRegister(&Alpha::GPRCRegClass);

  BuildMI(llscMBB, dl, TII->get(is32 ? Alpha::LDL_L : Alpha::LDQ_L),
          reg_res).addImm(0).addReg(reg_ptr);
  switch (MI.getOpcode()) {
  case Alpha::CAS32:
  case Alpha::CAS64: {
    unsigned reg_cmp
      = F->getRegInfo().createVirtualRegister(&Alpha::GPRCRegClass);
    BuildMI(llscMBB, dl, TII->get(Alpha::CMPEQ), reg_cmp)
      .addReg(reg_v2).addReg(reg_res);
    BuildMI(llscMBB, dl, TII->get(Alpha::BEQ))
      .addImm(0).addReg(reg_cmp).addMBB(sinkMBB);
    BuildMI(llscMBB, dl, TII->get(Alpha::BISr), reg_store)
      .addReg(Alpha::R31).addReg(MI.getOperand(3).getReg());
    break;
  }
  case Alpha::LAS32:
  case Alpha::LAS64: {
    BuildMI(llscMBB, dl,TII->get(is32 ? Alpha::ADDLr : Alpha::ADDQr), reg_store)
      .addReg(reg_res).addReg(reg_v2);
    break;
  }
  case Alpha::SWAP32:
  case Alpha::SWAP64: {
    BuildMI(llscMBB, dl, TII->get(Alpha::BISr), reg_store)
      .addReg(reg_v2).addReg(reg_v2);
    break;
  }
  }
  BuildMI(llscMBB, dl, TII->get(is32 ? Alpha::STL_C : Alpha::STQ_C), reg_store)
    .addReg(reg_store).addImm(0).addReg(reg_ptr);
  BuildMI(llscMBB, dl, TII->get(Alpha::BEQ))
    .addImm(0).addReg(reg_store).addMBB(llscMBB);
  BuildMI(llscMBB, dl, TII->get(Alpha::BR)).addMBB(sinkMBB);

  thisMBB->addSuccessor(llscMBB);
  llscMBB->addSuccessor(llscMBB);
  llscMBB->addSuccessor(sinkMBB);
  MI.eraseFromParent();   // The pseudo instruction is gone now.

  return sinkMBB;
}

bool
AlphaTargetLowering::isOffsetFoldingLegal(const GlobalAddressSDNode *GA) const {
  // The Alpha target isn't yet aware of offsets.
  return false;
}

bool AlphaTargetLowering::isFPImmLegal(const APFloat &Imm, EVT VT, bool ForCodeSize) const {
  if (VT != MVT::f32 && VT != MVT::f64)
    return false;
  // +0.0   F31
  // +0.0f  F31
  // -0.0  -F31
  // -0.0f -F31
  return Imm.isZero() || Imm.isNegZero();
}

bool AlphaTargetLowering::shouldInsertFencesForAtomic(const Instruction *I) const {
  return true;
}
