#!/bin/sh

set -xe

mkdir -p build

cc -D NIBBLE_ENABLE_UNITY_BUILD -Wall -Wextra -Wpedantic -O2 -I./src -o nibble ./src/main.c

cd tests/test_programs/libs
./build.sh
