add_llvm_component_library(LLVMAlphaDesc
  AlphaMCTargetDesc.cpp
  AlphaMCAsmInfo.cpp

  LINK_COMPONENTS
  AlphaInfo
  MC
  Support

  ADD_TO_COMPONENT
  Alpha
  )
