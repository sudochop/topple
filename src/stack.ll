@mem   = global [10000000 x i8] undef ; 10MB
@stack = global [1000000 x i64] undef  ; 8MB
@sp    = global i64 undef;

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

;define void @mem_set(i64 %idx, i8 %val) {
;    %addr = getelementptr [10000000 x i8], [10000000 x i8]* @mem, i8 0, i64 %idx
;
;    store i8 %val, i8* %addr
;
;    ret void
;}