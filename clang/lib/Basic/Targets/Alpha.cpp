#include "Alpha.h"

using namespace clang;
using namespace clang::targets;

AlphaTargetInfo::AlphaTargetInfo(const llvm::Triple &Triple, const TargetOptions &)
    : TargetInfo(Triple) {
  LongDoubleWidth = 128;
  LongDoubleAlign = 128;
  LongDoubleFormat = &llvm::APFloat::IEEEquad();
  SuitableAlign = 128;
  WIntType = UnsignedInt;
  LongWidth = LongAlign = PointerWidth = PointerAlign = 64;
  IntMaxType = Int64Type = SignedLong;
  resetDataLayout("e-m:m-f128:128:128-n64");
}

ArrayRef<const char *> AlphaTargetInfo::getGCCRegNames() const {
  static const char *const GCCRegNames[] = {
      "$0", "$1", "$2", "$3", "$4", "$5", "$6", "$7",
      "$8", "$9", "$10", "$11", "$12", "$13", "$14", "$15",
      "$16", "$17", "$18", "$19", "$20", "$21", "$22", "$23",
      "$24", "$25", "$26", "$27", "$28", "$29", "$30", "$31"};
  return llvm::makeArrayRef(GCCRegNames);
}