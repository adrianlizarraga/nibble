#!/bin/sh

set -xe

mkdir -p build

cc -Wall -Wextra -Wpedantic -O2 -I./src -o nibble ./src/main.c

cd tests/libs
./build.sh
