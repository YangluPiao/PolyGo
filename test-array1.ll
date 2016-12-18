; ModuleID = 'PolyGo'

@fmt = private unnamed_addr constant [4 x i8] c"%d\0A\00"
@float = private unnamed_addr constant [4 x i8] c"%f\0A\00"
@str = private unnamed_addr constant [4 x i8] c"%s\0A\00"
@real = private unnamed_addr constant [6 x i8] c"%.3f+\00"
@image = private unnamed_addr constant [7 x i8] c"%.3fi\0A\00"

declare i32 @printf(i8*, ...)

define i32 @main() {
entry:
  %c = alloca double, i32 2
  %comp_addr = getelementptr inbounds double, double* %c, i32 0
  %comp_addr1 = getelementptr inbounds double, double* %c, i32 1
  store double 0.000000e+00, double* %comp_addr
  store double 0.000000e+00, double* %comp_addr1
  ret i32 0
}
