# REQUIRES: x86
# RUN: rm -rf %t; split-file %s %t

# RUN: llvm-mc -filetype=obj -triple=x86_64-apple-darwin %t/libtlv.s -o %t/libtlv.o
# RUN: %lld -dylib -install_name @executable_path/libtlv.dylib \
# RUN:   -lSystem -o %t/libtlv.dylib %t/libtlv.o
# RUN: llvm-objdump --macho --exports-trie --rebase %t/libtlv.dylib | \
# RUN:   FileCheck %s --check-prefix=DYLIB
# DYLIB-DAG: _foo [per-thread]
# DYLIB-DAG: _bar [per-thread]
## Make sure we don't emit rebase opcodes for relocations in __thread_vars.
# DYLIB:       Rebase table:
# DYLIB-NEXT:  segment  section            address     type
# DYLIB-EMPTY:

# RUN: llvm-mc -filetype=obj -triple=x86_64-apple-darwin %t/test.s -o %t/test.o
# RUN: %lld -lSystem -L%t -ltlv %t/test.o -o %t/test
# RUN: llvm-objdump --bind -d --no-show-raw-insn %t/test | FileCheck %s

# CHECK:      movq [[#]](%rip), %rax # [[#%x, FOO:]]
# CHECK-NEXT: movq [[#]](%rip), %rax # [[#%x, BAR:]]
# CHECK-NEXT: movq [[#]](%rip), %rax # [[#%x, BAZ:]]

# CHECK-LABEL: Bind table:
# CHECK-DAG: __DATA       __thread_ptrs  0x{{0*}}[[#%x, FOO]] pointer 0   libtlv   _foo
# CHECK-DAG: __DATA       __thread_ptrs  0x{{0*}}[[#%x, BAR]] pointer 0   libtlv   _bar
# CHECK-DAG: __DATA_CONST __got          0x{{0*}}[[#%x, BAZ]] pointer 0   libtlv   _baz

## Check `type` on the various TLV sections, and check that
## nothing's after S_THREAD_LOCAL_ZEROFILL.
# RUN: llvm-otool -lv %t/test | FileCheck --check-prefix=FLAGS %s
# FLAGS:       sectname __got
# FLAGS-NEXT:   segname __DATA_CONST
# FLAGS-NEXT:      addr
# FLAGS-NEXT:      size 0x0000000000000008
# FLAGS-NEXT:    offset
# FLAGS-NEXT:     align 2^3 (8)
# FLAGS-NEXT:    reloff 0
# FLAGS-NEXT:    nreloc 0
# FLAGS-NEXT:      type S_NON_LAZY_SYMBOL_POINTERS
# FLAGS:       sectname __thread_vars
# FLAGS-NEXT:   segname __DATA
# FLAGS-NEXT:      addr
# FLAGS-NEXT:      size 0x0000000000000030
# FLAGS-NEXT:    offset
# FLAGS-NEXT:     align
# FLAGS-NEXT:    reloff 0
# FLAGS-NEXT:    nreloc 0
# FLAGS-NEXT:      type S_THREAD_LOCAL_VARIABLES
# FLAGS:       sectname __thread_ptrs
# FLAGS-NEXT:   segname __DATA
# FLAGS-NEXT:      addr
# FLAGS-NEXT:      size 0x0000000000000010
# FLAGS-NEXT:    offset
# FLAGS-NEXT:     align 2^3 (8)
# FLAGS-NEXT:    reloff 0
# FLAGS-NEXT:    nreloc 0
# FLAGS-NEXT:      type S_THREAD_LOCAL_VARIABLE_POINTERS
# FLAGS:       sectname __thread_data
# FLAGS-NEXT:   segname __DATA
# FLAGS-NEXT:      addr
# FLAGS-NEXT:      size 0x0000000000000008
# FLAGS-NEXT:    offset
# FLAGS-NEXT:     align
# FLAGS-NEXT:    reloff 0
# FLAGS-NEXT:    nreloc 0
# FLAGS-NEXT:      type S_THREAD_LOCAL_REGULAR
# FLAGS:       sectname __thread_bss
# FLAGS-NEXT:   segname __DATA
# FLAGS-NEXT:      addr
# FLAGS-NEXT:      size 0x0000000000000008
# FLAGS-NEXT:    offset
# FLAGS-NEXT:     align 2^3 (8)
# FLAGS-NEXT:    reloff 0
# FLAGS-NEXT:    nreloc 0
# FLAGS-NEXT:      type S_THREAD_LOCAL_ZEROFILL

#--- libtlv.s
.section __DATA,__thread_vars,thread_local_variables
.globl _foo, _bar, _baz
_foo:
_bar:

.text
_baz:

#--- test.s
.globl _main
_main:
  mov _foo@TLVP(%rip), %rax
  mov _bar@TLVP(%rip), %rax
## Add a GOT entry to make sure we don't mix it up with TLVs
  mov _baz@GOTPCREL(%rip), %rax
  ret

## Add some TLVs to test too, so that we can test the ordering
## of __thread_ptrs, __thread_data, and __thread_bss.
.section __DATA,__thread_data,thread_local_regular
_tfoo$tlv$init:
  .quad 123

.tbss _tbaz$tlv$init, 8, 3

.section __DATA,__thread_vars,thread_local_variables
.globl  _tfoo, _tbar
_tfoo:
  .quad  __tlv_bootstrap
  .quad  0
  .quad  _tfoo$tlv$init
_tbaz:
  .quad  __tlv_bootstrap
  .quad  0
  .quad  _tbaz$tlv$init
