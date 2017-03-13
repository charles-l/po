#!/bin/sh
csi -s bytecode.scm > t.s && nasm -felf32 t.s && ld t.o -dynamic-linker /lib32/ld-linux.so.2 -lc -melf_i386
