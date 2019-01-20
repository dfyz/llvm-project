set(LLVM_TARGET_DEFINITIONS Alpha.td)

tablegen(LLVM AlphaGenRegisterInfo.inc -gen-register-info)
tablegen(LLVM AlphaGenInstrInfo.inc -gen-instr-info)
tablegen(LLVM AlphaGenAsmWriter.inc -gen-asm-writer)
tablegen(LLVM AlphaGenDAGISel.inc -gen-dag-isel)
tablegen(LLVM AlphaGenCallingConv.inc -gen-callingconv)
tablegen(LLVM AlphaGenSubtargetInfo.inc -gen-subtarget)
add_public_tablegen_target(AlphaCommonTableGen)

add_llvm_target(AlphaCodeGen
  AlphaAsmPrinter.cpp
  AlphaBranchSelector.cpp
  AlphaInstrInfo.cpp
  AlphaISelDAGToDAG.cpp
  AlphaISelLowering.cpp
  AlphaFrameLowering.cpp
  AlphaLLRP.cpp
  AlphaRegisterInfo.cpp
  AlphaSubtarget.cpp
  AlphaTargetMachine.cpp
  )

add_subdirectory(TargetInfo)
add_subdirectory(MCTargetDesc)
