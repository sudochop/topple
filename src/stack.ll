@mem   = global [10000000 x i8] undef ; 10MB
@stack = global [1000000 x i64] undef  ; 8MB
@sp    = global i64 undef;

@int_formatter = private unnamed_addr constant [3 x i8] c"%d\00", align 1

declare i64 @putchar(i64)
declare i8 @printf(i8*, ...)

define void @push(i64 %val) {
    %sp   = load i64, i64* @sp
    %addr = getelementptr [1000000 x i64], [1000000 x i64]* @stack, i64 0, i64 %sp

    store i64 %val, i64* %addr

    %newsp = add i64 %sp, 1
    store i64 %newsp, i64* @sp

    ret void
}

define i64 @peek() {
    %sp    = load i64, i64* @sp
    %topsp = sub i64 %sp, 1
    %addr  = getelementptr [1000000 x i64], [1000000 x i64]* @stack, i64 0, i64 %topsp
    %val   = load i64, i64* %addr

    ret i64 %val
}

define i64 @pop() {
    %val = call i64 @peek()
    
    %sp    = load i64, i64* @sp
    %newsp = sub i64 %sp, 1
    store i64 %newsp, i64* @sp

    ret i64 %val
}

define i8* @mem_ptr() {
    %addr = getelementptr [10000000 x i8], [10000000 x i8]* @mem, i8 0, i8 0
    ret i8* %addr
}

define void @dbg() {
    call i64 (i64) @putchar(i64 40) ; '('
    call i64 (i64) @putchar(i64 32) ; ' '

    %sp    = load i64, i64* @sp
    %idxaddr = alloca i64
    store i64 0, i64* %idxaddr
    br label %check

check:
    %idx = load i64, i64* %idxaddr
    %comp = icmp ult i64 %idx, %sp
    br i1 %comp, label %body, label %cont

body:
    %addr  = getelementptr [1000000 x i64], [1000000 x i64]* @stack, i64 0, i64 %idx
    %val   = load i64, i64* %addr

    call i8 (i8*, ...) @printf(i8* getelementptr inbounds ([3 x i8], [3 x i8]* @int_formatter, i32 0, i32 0), i64 %val)
    call i64 (i64) @putchar(i64 32) ; ' '

    %newidx = add i64 %idx, 1
    store i64 %newidx, i64* %idxaddr
    br label %check

cont:
    call i64 (i64) @putchar(i64 45) ; '-'
    call i64 (i64) @putchar(i64 45) ; '-'
    call i64 (i64) @putchar(i64 32) ; ' '
    call i64 (i64) @putchar(i64 41) ; ')'
    call i64 (i64) @putchar(i64 10) ; '\n'
    ret void
}