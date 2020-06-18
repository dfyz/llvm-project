; RUN: llc -march=alpha < %s | FileCheck %s

target datalayout = "e-m:m-f128:128:128-n64"
target triple = "alpha-unknown-linux-gnu"

define i32 @fib_rec(i32 %n) noinline optnone {
 entry:
   %retval = alloca i32, align 4
   %n.addr = alloca i32, align 4
   store i32 %n, i32* %n.addr, align 4
   %0 = load i32, i32* %n.addr, align 4
   %cmp = icmp sle i32 %0, 0
   br i1 %cmp, label %if.then, label %if.end

 if.then:                                          ; preds = %entry
   store i32 0, i32* %retval, align 4
   br label %return

if.end:                                           ; preds = %entry
  %1 = load i32, i32* %n.addr, align 4
  %cmp1 = icmp eq i32 %1, 1
  br i1 %cmp1, label %if.then2, label %if.end3

if.then2:                                         ; preds = %if.end
; CHECK: $BB0_3:
  store i32 1, i32* %retval, align 4
  br label %return

if.end3:                                          ; preds = %if.end
  %2 = load i32, i32* %n.addr, align 4
  %sub = sub nsw i32 %2, 1
  %call = call i32 @fib_rec(i32 %sub)
  %3 = load i32, i32* %n.addr, align 4
  %sub4 = sub nsw i32 %3, 2
  %call5 = call i32 @fib_rec(i32 %sub4)
  %add = add nsw i32 %call, %call5
  store i32 %add, i32* %retval, align 4
  br label %return

return:                                           ; preds = %if.end3, %if.then2, %if.then
  %4 = load i32, i32* %retval, align 4
  ret i32 %4
}
