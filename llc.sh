#!/bin/bash
llvm-link src/stack.ll "out/$1.ll" -S > "out/$1-linked.ll" && \
llc -filetype=obj -O0 "out/$1-linked.ll" -o "out/$1.o" && \
clang "out/$1.o" -o "out/$1" && \
exec "./out/$1"
