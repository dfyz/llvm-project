add_llvm_library(LLVMAlphaInfo
  AlphaTargetInfo.cpp
  )

add_dependencies(LLVMAlphaInfo AlphaCommonTableGen)
