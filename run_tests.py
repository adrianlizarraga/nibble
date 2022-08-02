#!/usr/bin/env python3

"""
Heavily inspired by Alexey Kutepov's 'test.py' script (MIT License) in his Porth repo:
https://gitlab.com/tsoding/porth/-/blob/master/test.py
"""

import sys
import os
from os import path
import subprocess
import shlex
from typing import List, BinaryIO, Tuple, Optional
from dataclasses import dataclass, field

NIBBLE_EXT = ".nib"
TEST_CASE_EXT = ".txt"

@dataclass
class TestCase:
    argv: List[str]
    stdin: bytes
    stdout: bytes
    stderr: bytes
    returncode: int

DEFAULT_TEST_CASE=TestCase(argv=[], stdin=bytes(), stdout=bytes(), stderr=bytes(), returncode=0)

def print_usage(exe_name: str):
    print(f"Usage: {exe_name} [CMD]")
    print("CMD:")
    print("    run [test_target]         Run tests in a directory (default ./tests/) or for a single nibble file.")
    print("")

def read_int(fhandle: BinaryIO, name: bytes) -> bytes:
    line = fhandle.readline()
    prefix = b":" + name + b" "

    if not line.startswith(prefix):
        raise Exception(f"Failed to parse field '{name}': invalid prefix '{prefix}'.")
    
    return int(line[len(prefix) : -1])

def write_int(fhandle: BinaryIO, name: bytes, value: int):
    fhandle.write(b":%s %d\n" % (name, value))

def write_bytes(fhandle: BinaryIO, name: bytes, blob: bytes):
    fhandle.write(b":%s %d\n" % (name, len(blob)))
    fhandle.write(blob)
    fhandle.write(b"\n")

def read_bytes(fhandle: BinaryIO, name: bytes) -> bytes:
    line = fhandle.readline()
    prefix = b":" + name + b" "

    if not line.startswith(prefix):
        raise Exception(f"Failed to parse field '{name}': invalid prefix '{prefix}'.")

    num_bytes = int(line[len(prefix) : -1])
    blob = fhandle.read(num_bytes)

    if fhandle.read(1) != b"\n":
        raise Exception(f"Failed to parse field '{name}': missing ending newline.")

    return blob

def load_test_case(test_case_path: str) -> tuple[Optional[TestCase], Optional[str]]:
    try:
        with open(test_case_path, "rb") as fhandle:
            argv = []
            argc = read_int(fhandle, b"argc")

            for i in range(argc):
                argv.append(read_bytes(fhandle, b"arg%d" % i).decode("utf-8"))

            stdin = read_bytes(fhandle, b"stdin")
            stdout = read_bytes(fhandle, b"stdout")
            stderr = read_bytes(fhandle, b"stderr")
            returncode = read_int(fhandle, b"returncode")

            return (TestCase(argv, stdin, stdout, stderr, returncode), None)
    except Exception as e:
        return (None, str(e))

def save_test_case(test_case_path: str, argv: List[str], stdin: bytes, stdout: bytes, stderr: bytes, returncode: int):
    with open(test_case_path, "wb") as fhandle:
        write_int(fhandle, b"argc", len(argv))

        for i, arg in enumerate(argv):
            write_bytes(fhandle, b"arg%d" % i, arg.encode("utf-8"))

        write_bytes(fhandle, b"stdin", stdin)
        write_bytes(fhandle, b"stdout", stdout)
        write_bytes(fhandle, b"stderr", stderr)
        write_int(fhandle, b"returncode", returncode)

@dataclass
class RunStats:
    compile_failed: List[tuple[str, str]] = field(default_factory=list)
    test_failed: List[tuple[str, str]] = field(default_factory=list)
    only_compiled: List[str] = field(default_factory=list)
    num_tests: int = 0

def run_cmd(cmd, **kwargs) -> subprocess.CompletedProcess:
    print("[CMD] %s" % " ".join(map(shlex.quote, cmd)))
    return subprocess.run(cmd, **kwargs)

def check_test_case(cmd_ret: subprocess.CompletedProcess, test_case: TestCase) -> Optional[str]:
    err = None

    if cmd_ret.returncode != test_case.returncode:
        err = f"Invalid return code. expected '{test_case.returncode}', but got '{cmd_ret.returncode}'."
    elif cmd_ret.stdout != test_case.stdout:
        err = f"Invalid stdout. expected:\n{test_case.stdout.decode('utf-8')}\ngot:\n{cmd_ret.stdout.decode('utf-8')}"
    elif cmd_ret.stderr != test_case.stderr:
        err = f"Invalid stderr. expected:\n{test_case.stderr.decode('utf-8')}\ngot\n{cmd_ret.stderr.decode('utf-8')}"

    return err

def run_file_test(file_path: str, stats: RunStats = RunStats()):
    test_case_path = file_path[:-len(NIBBLE_EXT)] + TEST_CASE_EXT
    test_case, err = load_test_case(test_case_path)

    if test_case is not None:
        # Compile nibble program first.
        cmd_ret = run_cmd(["./nibble", "-s", file_path], capture_output=True)

        # Run nibble program if it compiled.
        if cmd_ret.returncode == 0:
            cmd_ret = run_cmd(["./out", *test_case.argv], input=test_case.stdin, capture_output=True)

        err = check_test_case(cmd_ret, test_case)

        if err:
            stats.test_failed.append((file_path, err))
    else:
        print(f"[WARNING] Failed to load test case file for {file_path}: {err}", file=sys.stderr)
        compile_ret = run_cmd(["./nibble", file_path], capture_output=True)

        if compile_ret.returncode != 0:
            stats.compile_failed.append((file_path, compile_ret.stderr.decode("utf-8")))
        else:
            stats.only_compiled.append(file_path)

    stats.num_tests += 1

def run_dir_tests(folder: str, stats: RunStats):
    for e in os.scandir(folder):
        if e.is_file() and e.path.endswith(NIBBLE_EXT):
            run_file_test(e.path, stats)

def update_file_output(file_path: str):
    test_case_path = file_path[:-len(NIBBLE_EXT)] + TEST_CASE_EXT
    test_case, err = load_test_case(test_case_path)

    if err is not None:
        test_case = DEFAULT_TEST_CASE

    compile_ret = run_cmd(["./nibble", "-s", file_path], capture_output=True)

    print(f"[INFO] Saving output to {test_case_path}")

    if compile_ret.returncode != 0: # Save compiler error
        save_test_case(test_case_path, test_case.argv, test_case.stdin, compile_ret.stdout, compile_ret.stderr, compile_ret.returncode)
    else:
        run_ret = run_cmd(["./out", *test_case.argv], input=test_case.stdin, capture_output=True)
        save_test_case(test_case_path, test_case.argv, test_case.stdin, run_ret.stdout, run_ret.stderr, run_ret.returncode)

def update_dir_outputs(folder: str):
    for e in os.scandir(folder):
        if e.is_file() and e.path.endswith(NIBBLE_EXT):
            update_file_output(e.path)

def main():
    exe_name, *argv = sys.argv

    cmd = "run"

    if len(argv) > 0:
        cmd, *argv = argv

    if cmd == "run":
        test_target = "./tests/"

        if len(argv) > 0:
            test_target, *argv = argv

        stats = RunStats()

        if path.isdir(test_target):
            run_dir_tests(test_target, stats)
        elif path.isfile(test_target):
            run_file_test(test_target, stats)
        else:
            print_usage(exe_name)
            print(f"[ERROR] Invalid test target '{test_target}'", file=sys.stderr)
            exit(1)

        num_compile_failed = len(stats.compile_failed)
        num_tests_failed = len(stats.test_failed)
        total_failed = num_compile_failed + num_tests_failed
        num_ignored = len(stats.only_compiled)

        if (total_failed > 0):
            print(f"Total failed: {total_failed}, Ignored: {num_ignored}\n", file=sys.stderr)

            if num_compile_failed:
                print(f"Compiler failures ({num_compile_failed}):", file=sys.stderr)

                for i, f in enumerate(stats.compile_failed):
                    print(f"[{i+1}/{num_compile_failed}]: {f[0]}:", file=sys.stderr)
                    print(f"{f[1]}\n", file=sys.stderr)

            if num_tests_failed:
                print(f"Test failures ({num_tests_failed}):", file=sys.stderr)

                for i, f in enumerate(stats.test_failed):
                    print(f"[{i+1}/{num_tests_failed}]: {f[0]}:", file=sys.stderr)
                    print(f"{f[1]}\n", file=sys.stderr)

            exit(1)
        else:
            print(f"\n[INFO] All {stats.num_tests} tests passed")
    elif cmd == "update_out":
        test_target = "./tests/"

        if len(argv) > 0:
            test_target, *argv = argv

        if path.isdir(test_target):
            update_dir_outputs(test_target)
        elif path.isfile(test_target):
            update_file_output(test_target)
        else:
            print_usage(exe_name)
            print(f"[ERROR] Invalid test target '{test_target}'", file=sys.stderr)
            exit(1)
    else:
        print_usage(exe_name)
        print(f"[ERROR] Unknown command '{cmd}'", file=sys.stderr)
        exit(1)



if __name__ == "__main__":
    main()
