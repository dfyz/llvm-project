//===- WasmObjcopy.cpp ----------------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "llvm/ObjCopy/wasm/WasmObjcopy.h"
#include "WasmObject.h"
#include "WasmReader.h"
#include "WasmWriter.h"
#include "llvm/BinaryFormat/Wasm.h"
#include "llvm/ObjCopy/CommonConfig.h"
#include "llvm/Support/Errc.h"
#include "llvm/Support/FileOutputBuffer.h"
#include "llvm/Support/LEB128.h"
#include "llvm/Support/SmallVectorMemoryBuffer.h"

namespace llvm {
namespace objcopy {
namespace wasm {

using namespace object;
using SectionPred = std::function<bool(const Section &Sec)>;

static bool isDebugSection(const Section &Sec) {
  return Sec.Name.starts_with(".debug");
}

static bool isLinkingSection(const Section &Sec) {
  return Sec.Name == "linking";
}

static bool isLinkerSection(const Section &Sec) {
  return Sec.Name.starts_with("reloc.") || isLinkingSection(Sec);
}

static bool isNameSection(const Section &Sec) { return Sec.Name == "name"; }

// Sections which are known to be "comments" or informational and do not affect
// program semantics.
static bool isCommentSection(const Section &Sec) {
  return Sec.Name == "producers";
}

static Expected<uint64_t> readVaruint(const uint8_t *&Cur, const uint8_t *End) {
  unsigned N;
  const char* Error = nullptr;
  uint64_t Res = decodeULEB128(Cur, &N, End, &Error);
  if (Error) {
    return createStringError(errc::invalid_argument, "failed to read a valid integer: %s",
                             Error);
  }
  Cur += N;
  return Res;
}

static Expected<StringRef> readString(const uint8_t *&Cur, const uint8_t *End) {
  auto LenOrErr = readVaruint(Cur, End);
  if (!LenOrErr) {
    return LenOrErr.takeError();
  }
  uint64_t Len = *LenOrErr;
  if (Cur + Len > End) {
    return createStringError(errc::invalid_argument, "failed to read a valid string");
  }
  StringRef Res{reinterpret_cast<const char *>(Cur), Len};
  Cur += Len;
  return Res;
}

static void processSymbol(uint64_t &Flags, StringRef &Name, const CommonConfig &Config) {
  if (Config.SymbolsToLocalize.matches(Name) ||
      (!Config.SymbolsToKeepGlobal.empty() && !Config.SymbolsToKeepGlobal.matches(Name))) {
    Flags |= llvm::wasm::WASM_SYMBOL_BINDING_LOCAL;
  }

  if (Config.SymbolsToGlobalize.matches(Name)) {
    Flags &= ~llvm::wasm::WASM_SYMBOL_BINDING_LOCAL;
  }

  const auto NewName = Config.SymbolsToRename.find(Name);
  if (NewName != Config.SymbolsToRename.end()) {
    Name = NewName->getValue();
  }
}

static Error rebuildSymbol(const uint8_t *&Cur, const uint8_t *End,
                           uint8_t Type, uint64_t Flags, raw_ostream &Out,
                           const CommonConfig &Config) {
  bool IsDefined = (Flags & llvm::wasm::WASM_SYMBOL_UNDEFINED) == 0;
  switch (Type) {
  case llvm::wasm::WASM_SYMBOL_TYPE_FUNCTION:
  case llvm::wasm::WASM_SYMBOL_TYPE_GLOBAL:
  case llvm::wasm::WASM_SYMBOL_TYPE_TAG:
  case llvm::wasm::WASM_SYMBOL_TYPE_TABLE:
    {
      auto Index = readVaruint(Cur, End);
      if (!Index) {
        return createStringError(errc::invalid_argument,
                                 "malformed symbol index");
      }

      StringRef Name;
      bool HasName = IsDefined ||
                     (Flags & llvm::wasm::WASM_SYMBOL_EXPLICIT_NAME) != 0;
      if (HasName) {
        auto NameOrErr = readString(Cur, End);
        if (!NameOrErr) {
          return createStringError(errc::invalid_argument,
                                   "malformed symbol name");
        }
        Name = *NameOrErr;
      }

      if (IsDefined) {
        processSymbol(Flags, Name, Config);
      }
      encodeULEB128(Flags, Out);
      encodeULEB128(*Index, Out);
      if (HasName) {
        encodeULEB128(Name.size(), Out);
        Out << Name;
      }
    }
    break;

  case llvm::wasm::WASM_SYMBOL_TYPE_DATA:
    {
      auto NameOrErr = readString(Cur, End);
      if (!NameOrErr) {
        return createStringError(errc::invalid_argument,
                                 "malformed data symbol name");
      }
      StringRef Name = *NameOrErr;

      if (IsDefined) {
        processSymbol(Flags, Name, Config);
      }

      encodeULEB128(Flags, Out);
      encodeULEB128(Name.size(), Out);
      Out << Name;

      if (IsDefined) {
        // Index, Offset, Size
        for (size_t I = 0; I < 3; I++) {
          auto IntOrErr = readVaruint(Cur, End);
          if (!IntOrErr) {
            return createStringError(errc::invalid_argument,
                                     "malformed data symbol");
          }
          encodeULEB128(*IntOrErr, Out);
        }
      }
    }
    break;

  case llvm::wasm::WASM_SYMBOL_TYPE_SECTION:
    {
      auto SectionOrErr = readVaruint(Cur, End);
      if (!SectionOrErr) {
        return createStringError(errc::invalid_argument,
                                 "malformed section symbol");
      }
      encodeULEB128(Flags, Out);
      encodeULEB128(*SectionOrErr, Out);
    }
    break;

  default:
    return createStringError(errc::invalid_argument,
                             "unknown symbol type: %hhu",
                             Type);
  }
  return Error::success();
}

static Error rebuildSymbolsSubsection(const uint8_t *&Cur, const uint8_t *End,
                                      raw_ostream &Out, const CommonConfig &Config) {
  auto CountOrErr = readVaruint(Cur, End);
  if (!CountOrErr) {
    return CountOrErr.takeError();
  }
  uint64_t Count = *CountOrErr;
  encodeULEB128(Count, Out);

  while (Count--) {
    if (Cur >= End) {
      return createStringError(errc::invalid_argument,
                               "unexpected end of the symbols subsection");
    }

    uint8_t SymbolType = *Cur++;
    Out.write(SymbolType);
    auto FlagsOrErr = readVaruint(Cur, End);
    if (!FlagsOrErr) {
      return FlagsOrErr.takeError();
    }
    if (Error E = rebuildSymbol(Cur, End, SymbolType, *FlagsOrErr, Out, Config)) {
      return E;
    }
  }

  return Error::success();
}

static Expected<std::unique_ptr<MemoryBuffer>> rebuildLinkingSection(Section &LinkingSec,
                                                                     const CommonConfig &Config) {
  SmallVector<char, 0> Buffer;
  raw_svector_ostream LinkingStream(Buffer);

  const uint8_t *Cur = LinkingSec.Contents.data();
  const uint8_t *End = Cur + LinkingSec.Contents.size();

  auto Version = readVaruint(Cur, End);
  if (!Version) {
    return Version.takeError();
  }
  // Assume the version has been validated elsewhere.
  encodeULEB128(*Version, LinkingStream);

  while (Cur < End) {
    uint8_t Type = *Cur++;
    LinkingStream.write(Type);

    auto Size = readVaruint(Cur, End);
    if (!Size) {
      return Size.takeError();
    }

    if (Type == llvm::wasm::WASM_SYMBOL_TABLE) {
      // Rebuild the symbols subsection from scratch.
      SmallVector<char, 0> SymbolsBuffer;
      raw_svector_ostream SymbolsStream(SymbolsBuffer);
      if (Error E = rebuildSymbolsSubsection(Cur, Cur + *Size, SymbolsStream, Config)) {
        return E;
      }
      encodeULEB128(SymbolsBuffer.size(), LinkingStream);
      LinkingStream << SymbolsBuffer;
    } else {
      // Just pass the existing subsection through.
      encodeULEB128(*Size, LinkingStream);

      if (Cur + *Size > End) {
        return createStringError(
            errc::invalid_argument,
            "malformed linking section: subsection %hhu has invalid size",
            Type);
      }
      LinkingStream.write(reinterpret_cast<const char *>(Cur), *Size);
      Cur += *Size;
    }
  }

  return std::make_unique<SmallVectorMemoryBuffer>(
      std::move(Buffer),
      /*RequiresNullTerminator=*/false);
}

static Error dumpSectionToFile(StringRef SecName, StringRef Filename,
                               Object &Obj) {
  for (const Section &Sec : Obj.Sections) {
    if (Sec.Name == SecName) {
      ArrayRef<uint8_t> Contents = Sec.Contents;
      Expected<std::unique_ptr<FileOutputBuffer>> BufferOrErr =
          FileOutputBuffer::create(Filename, Contents.size());
      if (!BufferOrErr)
        return BufferOrErr.takeError();
      std::unique_ptr<FileOutputBuffer> Buf = std::move(*BufferOrErr);
      std::copy(Contents.begin(), Contents.end(), Buf->getBufferStart());
      if (Error E = Buf->commit())
        return E;
      return Error::success();
    }
  }
  return createStringError(errc::invalid_argument, "section '%s' not found",
                           SecName.str().c_str());
}

static void removeSections(const CommonConfig &Config, Object &Obj) {
  SectionPred RemovePred = [](const Section &) { return false; };

  // Explicitly-requested sections.
  if (!Config.ToRemove.empty()) {
    RemovePred = [&Config](const Section &Sec) {
      return Config.ToRemove.matches(Sec.Name);
    };
  }

  if (Config.StripDebug) {
    RemovePred = [RemovePred](const Section &Sec) {
      return RemovePred(Sec) || isDebugSection(Sec);
    };
  }

  if (Config.StripAll) {
    RemovePred = [RemovePred](const Section &Sec) {
      return RemovePred(Sec) || isDebugSection(Sec) || isLinkerSection(Sec) ||
             isNameSection(Sec) || isCommentSection(Sec);
    };
  }

  if (Config.OnlyKeepDebug) {
    RemovePred = [&Config](const Section &Sec) {
      // Keep debug sections, unless explicitly requested to remove.
      // Remove everything else, including known sections.
      return Config.ToRemove.matches(Sec.Name) || !isDebugSection(Sec);
    };
  }

  if (!Config.OnlySection.empty()) {
    RemovePred = [&Config](const Section &Sec) {
      // Explicitly keep these sections regardless of previous removes.
      // Remove everything else, inluding known sections.
      return !Config.OnlySection.matches(Sec.Name);
    };
  }

  if (!Config.KeepSection.empty()) {
    RemovePred = [&Config, RemovePred](const Section &Sec) {
      // Explicitly keep these sections regardless of previous removes.
      if (Config.KeepSection.matches(Sec.Name))
        return false;
      // Otherwise defer to RemovePred.
      return RemovePred(Sec);
    };
  }

  Obj.removeSections(RemovePred);
}

static Error handleArgs(const CommonConfig &Config, Object &Obj) {
  // Only support AddSection, DumpSection, RemoveSection for now.
  // Also support a subset of options for symbol processing.
  for (Section &Sec : Obj.Sections) {
    if (isLinkingSection(Sec)) {
      Expected<std::unique_ptr<MemoryBuffer>> NewContent =
          rebuildLinkingSection(Sec, Config);
      if (!NewContent) {
        return NewContent.takeError();
      }
      Obj.replaceSectionContent(Sec, std::move(*NewContent));
    }
  }

  for (StringRef Flag : Config.DumpSection) {
    StringRef SecName;
    StringRef FileName;
    std::tie(SecName, FileName) = Flag.split("=");
    if (Error E = dumpSectionToFile(SecName, FileName, Obj))
      return createFileError(FileName, std::move(E));
  }

  removeSections(Config, Obj);

  for (const NewSectionInfo &NewSection : Config.AddSection) {
    Section Sec;
    Sec.SectionType = llvm::wasm::WASM_SEC_CUSTOM;
    Sec.Name = NewSection.SectionName;

    llvm::StringRef InputData =
        llvm::StringRef(NewSection.SectionData->getBufferStart(),
                        NewSection.SectionData->getBufferSize());
    std::unique_ptr<MemoryBuffer> BufferCopy = MemoryBuffer::getMemBufferCopy(
        InputData, NewSection.SectionData->getBufferIdentifier());
    Sec.Contents = ArrayRef<uint8_t>(
        reinterpret_cast<const uint8_t *>(BufferCopy->getBufferStart()),
        BufferCopy->getBufferSize());

    Obj.addSectionWithOwnedContents(Sec, std::move(BufferCopy));
  }

  return Error::success();
}

Error executeObjcopyOnBinary(const CommonConfig &Config, const WasmConfig &,
                             object::WasmObjectFile &In, raw_ostream &Out) {
  Reader TheReader(In);
  Expected<std::unique_ptr<Object>> ObjOrErr = TheReader.create();
  if (!ObjOrErr)
    return createFileError(Config.InputFilename, ObjOrErr.takeError());
  Object *Obj = ObjOrErr->get();
  assert(Obj && "Unable to deserialize Wasm object");
  if (Error E = handleArgs(Config, *Obj))
    return E;
  Writer TheWriter(*Obj, Out);
  if (Error E = TheWriter.write())
    return createFileError(Config.OutputFilename, std::move(E));
  return Error::success();
}

} // end namespace wasm
} // end namespace objcopy
} // end namespace llvm
