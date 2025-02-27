//===- OutputSegment.cpp --------------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "OutputSegment.h"
#include "ConcatOutputSection.h"
#include "InputSection.h"
#include "SyntheticSections.h"

#include "lld/Common/ErrorHandler.h"
#include "lld/Common/Memory.h"
#include "llvm/ADT/StringSwitch.h"
#include "llvm/BinaryFormat/MachO.h"

using namespace llvm;
using namespace llvm::MachO;
using namespace lld;
using namespace lld::macho;

static uint32_t initProt(StringRef name) {
  auto it = find_if(
      config->segmentProtections,
      [&](const SegmentProtection &segprot) { return segprot.name == name; });
  if (it != config->segmentProtections.end())
    return it->initProt;

  if (name == segment_names::text)
    return VM_PROT_READ | VM_PROT_EXECUTE;
  if (name == segment_names::pageZero)
    return 0;
  if (name == segment_names::linkEdit)
    return VM_PROT_READ;
  return VM_PROT_READ | VM_PROT_WRITE;
}

static uint32_t maxProt(StringRef name) {
  assert(config->arch() != AK_i386 &&
         "TODO: i386 has different maxProt requirements");
  return initProt(name);
}

size_t OutputSegment::numNonHiddenSections() const {
  size_t count = 0;
  for (const OutputSection *osec : sections)
    count += (!osec->isHidden() ? 1 : 0);
  return count;
}

void OutputSegment::addOutputSection(OutputSection *osec) {
  inputOrder = std::min(inputOrder, osec->inputOrder);

  osec->parent = this;
  sections.push_back(osec);

  for (const SectionAlign &sectAlign : config->sectionAlignments)
    if (sectAlign.segName == name && sectAlign.sectName == osec->name)
      osec->align = sectAlign.align;
}

template <typename T, typename F> static auto compareByOrder(F ord) {
  return [=](T a, T b) { return ord(a) < ord(b); };
}

static int segmentOrder(OutputSegment *seg) {
  return StringSwitch<int>(seg->name)
      .Case(segment_names::pageZero, -4)
      .Case(segment_names::text, -3)
      .Case(segment_names::dataConst, -2)
      .Case(segment_names::data, -1)
      .Case(segment_names::llvm, std::numeric_limits<int>::max() - 1)
      // Make sure __LINKEDIT is the last segment (i.e. all its hidden
      // sections must be ordered after other sections).
      .Case(segment_names::linkEdit, std::numeric_limits<int>::max())
      .Default(seg->inputOrder);
}

static int sectionOrder(OutputSection *osec) {
  StringRef segname = osec->parent->name;
  // Sections are uniquely identified by their segment + section name.
  if (segname == segment_names::text) {
    return StringSwitch<int>(osec->name)
        .Case(section_names::header, -4)
        .Case(section_names::text, -3)
        .Case(section_names::stubs, -2)
        .Case(section_names::stubHelper, -1)
        .Case(section_names::unwindInfo, std::numeric_limits<int>::max() - 1)
        .Case(section_names::ehFrame, std::numeric_limits<int>::max())
        .Default(osec->inputOrder);
  } else if (segname == segment_names::data ||
             segname == segment_names::dataConst) {
    // For each thread spawned, dyld will initialize its TLVs by copying the
    // address range from the start of the first thread-local data section to
    // the end of the last one. We therefore arrange these sections contiguously
    // to minimize the amount of memory used. Additionally, since zerofill
    // sections must be at the end of their segments, and since TLV data
    // sections can be zerofills, we end up putting all TLV data sections at the
    // end of the segment.
    switch (sectionType(osec->flags)) {
    case S_THREAD_LOCAL_VARIABLE_POINTERS:
      return std::numeric_limits<int>::max() - 3;
    case S_THREAD_LOCAL_REGULAR:
      return std::numeric_limits<int>::max() - 2;
    case S_THREAD_LOCAL_ZEROFILL:
      return std::numeric_limits<int>::max() - 1;
    case S_ZEROFILL:
      return std::numeric_limits<int>::max();
    default:
      return StringSwitch<int>(osec->name)
          .Case(section_names::got, -3)
          .Case(section_names::lazySymbolPtr, -2)
          .Case(section_names::const_, -1)
          .Default(osec->inputOrder);
    }
  } else if (segname == segment_names::linkEdit) {
    return StringSwitch<int>(osec->name)
        .Case(section_names::rebase, -10)
        .Case(section_names::binding, -9)
        .Case(section_names::weakBinding, -8)
        .Case(section_names::lazyBinding, -7)
        .Case(section_names::export_, -6)
        .Case(section_names::functionStarts, -5)
        .Case(section_names::dataInCode, -4)
        .Case(section_names::symbolTable, -3)
        .Case(section_names::indirectSymbolTable, -2)
        .Case(section_names::stringTable, -1)
        .Case(section_names::codeSignature, std::numeric_limits<int>::max())
        .Default(osec->inputOrder);
  }
  // ZeroFill sections must always be the at the end of their segments,
  // otherwise subsequent sections may get overwritten with zeroes at runtime.
  if (sectionType(osec->flags) == S_ZEROFILL)
    return std::numeric_limits<int>::max();
  return osec->inputOrder;
}

void OutputSegment::sortOutputSections() {
  llvm::sort(sections, compareByOrder<OutputSection *>(sectionOrder));
}

void macho::sortOutputSegments() {
  llvm::sort(outputSegments, compareByOrder<OutputSegment *>(segmentOrder));
}

static DenseMap<StringRef, OutputSegment *> nameToOutputSegment;
std::vector<OutputSegment *> macho::outputSegments;

OutputSegment *macho::getOrCreateOutputSegment(StringRef name) {
  OutputSegment *&segRef = nameToOutputSegment[name];
  if (segRef)
    return segRef;

  segRef = make<OutputSegment>();
  segRef->name = name;
  segRef->maxProt = maxProt(name);
  segRef->initProt = initProt(name);

  outputSegments.push_back(segRef);
  return segRef;
}
