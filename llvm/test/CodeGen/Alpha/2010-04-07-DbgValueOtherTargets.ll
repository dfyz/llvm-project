RUN: llc -O0 -march=alpha -asm-verbose %S/../Inputs/DbgValueOtherTargets.ll -o - | FileCheck %S/../Inputs/DbgValueOtherTargets.ll
