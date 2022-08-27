#!/bin/sh

set -xe

mkdir -p build

cc -Wall -Wextra -Wpedantic -g -I./src -o nibble ./src/main.c

cd tests/libs
./build.sh
