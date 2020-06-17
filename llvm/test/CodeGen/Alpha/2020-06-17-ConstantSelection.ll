; RUN: llc -march=alpha < %s | FileCheck %s

define i64 @zero() {
; CHECK: bis $31,$31,$0
  ret i64 0
}

; A single LDA
define i64 @sext16_1() {
; CHECK: lda $0,42($31)
  ret i64 42
}

define i64 @sext16_2() {
; CHECK: lda $0,-1($31)
  ret i64 -1
}

define i64 @sext16_3() {
; CHECK: lda $0,32767($31)
  ret i64 32767
}

; LDA + zeroing out the top bytes
define i64 @sext16_int_1() {
; CHECK: lda $0,-1($31)
; CHECK-NEXT: zapnot $0,15,$0
  ret i64 4294967295
}

define i64 @sext16_int_2() {
; CHECK: lda $0,-32477($31)
; CHECK-NEXT: zapnot $0,15,$0
  ret i64 4294934819
}

; LDA + LDAH
define i64 @const2part_1() {
; CHECK: ldah $0,19($31)
; CHECK-NEXT: lda $0,-10617($0)
  ret i64 1234567
}

define i64 @const2part_2() {
; CHECK: ldah $0,-19($31)
; CHECK-NEXT: lda $0,10617($0)
  ret i64 -1234567
}

define i64 @const2part_3() {
; CHECK: ldah $0,-32768($31)
; CHECK-NEXT: lda $0,-32768($0)
  ret i64 -2147516416
}

define i64 @const2part_4() {
; CHECK: ldah $0,32767($31)
; CHECK-NEXT: lda $0,32767($0)
  ret i64 2147450879
}

; LDA + LDAH + zeroing out the top bytes
define i64 @const2part_int_1() {
; CHECK: ldah $0,-32768($31)
; CHECK-NEXT: lda $0,-50($0)
; CHECK-NEXT: zapnot $0,15,$0
  ret i64 2147483598
}

define i64 @const2part_int_2() {
; CHECK: ldah $0,-14673($31)
; CHECK-NEXT: lda $0,-24235($0)
; CHECK-NEXT: zapnot $0,15,$0
  ret i64 3333333333
}

define i64 @const2part_int_3() {
; CHECK: ldah $0,-4113($31)
; CHECK-NEXT: lda $0,-4370($0)
; CHECK-NEXT: zapnot $0,15,$0
  ret i64 4025413358
}

; Can't be materialized from immediates
define i64 @relpool() {
; CHECK: .quad  4294967296
; CHECK: ldah {{.*}} !gprelhigh
; CHECK-NEXT: ldq {{.*}} !gprellow
  ret i64 4294967296
}