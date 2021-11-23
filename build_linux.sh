#!/bin/sh

set -xe

mkdir -p build

cc -Wall -Wextra -Wpedantic -g -I./src -o build/nibble ./src/main.c
