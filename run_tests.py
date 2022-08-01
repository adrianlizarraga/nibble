#!/usr/bin/env python3

import sys
import os
from os import path
import subprocess
import shlex
from typing import List, BinaryIO, Tuple, Optional
from dataclasses import dataclass

@dataclass
class TestCase:
    argv: List[str]
    stdin: bytes
    stdout: bytes
    stderr: bytes
    returncode: int

def print_usage(exe_name):
    print(f"Usage: {exe_name} [CMD]")
    print("CMD:")
    print("    run <test_target_dir>         Run tests in target directory (or nibble file)")
    print("")

def main():
    exe_name, *argv = sys.argv

    cmd = "run"

    if len(argv) > 0:
        cmd, *argv = argv

    if cmd == "run":

    else:
        print_usage(exe_name)
        print(f"[ERROR]: Unknown command '{cmd}'", file=sys.stderr) 
        exit(1)



if __name__ == "__main__":
    main()
