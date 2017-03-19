#!/bin/sh

cc -c -g -m32 boot.c symtab.c &&
    csi -s bytecode.scm > t.s && nasm -felf32 t.s &&
    cc -m32 -o po boot.o symtab.o t.o
