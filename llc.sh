#!/bin/bash
llvm-link src/stack.ll "out/$1.ll" -S > "out/$1-linked.ll" && \
opt -S -mem2reg "out/$1-linked.ll" -o "out/$1-linked-opt.ll" &&
llc -filetype=obj -O3 "out/$1-linked-opt.ll" -o "out/$1.o" && \
clang "out/$1.o" -o "out/$1" && \
exec "./out/$1"
