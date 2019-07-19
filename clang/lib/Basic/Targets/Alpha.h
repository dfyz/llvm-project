#ifndef LLVM_CLANG_LIB_BASIC_TARGETS_ALPHA_H
#define LLVM_CLANG_LIB_BASIC_TARGETS_ALPHA_H

#include "clang/Basic/TargetInfo.h"
#include "clang/Basic/TargetOptions.h"

namespace clang {
namespace targets {

class AlphaTargetInfo : public TargetInfo {
public:
  AlphaTargetInfo(const llvm::Triple &Triple, const TargetOptions &);

  void getTargetDefines(const LangOptions &Opts, MacroBuilder &Builder) const override {}

  ArrayRef<Builtin::Info> getTargetBuiltins() const override { return None; }

  BuiltinVaListKind getBuiltinVaListKind() const override {
    // TODO: add a proper type.
    return TargetInfo::VoidPtrBuiltinVaList;
  }

  bool validateAsmConstraint(const char *&,
                             TargetInfo::ConstraintInfo &) const override {
    // No support for asm constraints at all so far.
    return false;
  }

  const char *getClobbers() const override { return ""; }

  ArrayRef<const char *> getGCCRegNames() const override;

  ArrayRef<TargetInfo::GCCRegAlias> getGCCRegAliases() const override {
    return None;
  }
};

}
}

#endif //LLVM_CLANG_LIB_BASIC_TARGETS_ALPHA_H
