#!/bin/sh

gcc add.c -c -o add.o
gcc sub.c -c -o sub.o
gcc mul.c -c -o mul.o
ar rcs libaddsub.a add.o sub.o
