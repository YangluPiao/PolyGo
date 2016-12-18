; ModuleID = 'PolyGo'

@fmt = private unnamed_addr constant [4 x i8] c"%d\0A\00"
@float = private unnamed_addr constant [4 x i8] c"%f\0A\00"
@str = private unnamed_addr constant [4 x i8] c"%s\0A\00"
@real = private unnamed_addr constant [6 x i8] c"%.3f+\00"
@image = private unnamed_addr constant [7 x i8] c"%.3fi\0A\00"
@fmt.1 = private unnamed_addr constant [4 x i8] c"%d\0A\00"
@float.2 = private unnamed_addr constant [4 x i8] c"%f\0A\00"
@str.3 = private unnamed_addr constant [4 x i8] c"%s\0A\00"
@real.4 = private unnamed_addr constant [6 x i8] c"%.3f+\00"
@image.5 = private unnamed_addr constant [7 x i8] c"%.3fi\0A\00"

declare i32 @printf(i8*, ...)

define i32 @main() {
entry:
  %a = alloca i32
  %b = alloca i32
  store i32 1, i32* %a
  store i32 2, i32* %b
  %add_result = call i32 @add(i32 1, i32 2)
  %printf = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @fmt, i32 0, i32 0), i32 %add_result)
  ret i32 0
}

define i32 @add(i32 %b, i32 %a) {
entry:
  %b1 = alloca i32
  store i32 %b, i32* %b1
  %a2 = alloca i32
  store i32 %a, i32* %a2
  %tmp = load i32, i32* %a2
  %tmp3 = load i32, i32* %b1
  %tmp4 = add i32 %tmp, %tmp3
  ret i32 %tmp4
}
