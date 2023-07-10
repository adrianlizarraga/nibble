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
    include_paths: List[str]
    lib_paths: List[str]
    argv: List[str]
    stdin: bytes
    stdout: bytes
    stderr: bytes
    returncode: int

DEFAULT_TEST_CASE=TestCase(include_paths=[], lib_paths=[], argv=[], stdin=bytes(), stdout=bytes(), stderr=bytes(), returncode=0)

def print_usage(prog_name: str):
    print(f"Usage: {prog_name} [CMD] [Additional Nibble compiler args]")
    print("CMD:")
    print("    --help                    Print this help message.")
    print("    --nibble_prog <path>      Path to the Nibble compiler program.")
    print("    --update_tests            Update expected test results.")
    print("    --test_target <path>      Directory containing tests to run or update. Can also be a single nibble file.")
    print("")

def read_int(fhandle: BinaryIO, name: bytes) -> bytes:
    line = fhandle.readline()
    prefix = b":" + name + b" "

    if not line.startswith(prefix):
        raise Exception(f"Failed to parse field '{name}': invalid prefix '{prefix}'.")
    
    return int(line[len(prefix) : -1])

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

def read_paths(fhandle: BinaryIO, name: bytes) -> List[str]:
    line = fhandle.readline()
    prefix = b":" + name

    if not line.startswith(prefix):
        raise Exception(f"Failed to parse field '{name}': invalid prefix '{prefix}'.")

    paths_str = line[len(prefix) + 1 : -1].decode("utf-8")
    return paths_str.split(" ") if paths_str else []

def write_int(fhandle: BinaryIO, name: bytes, value: int):
    fhandle.write(b":%s %d\n" % (name, value))

def write_bytes(fhandle: BinaryIO, name: bytes, blob: bytes):
    fhandle.write(b":%s %d\n" % (name, len(blob)))
    fhandle.write(blob)
    fhandle.write(b"\n")

def write_paths(fhandle: BinaryIO, name: bytes, paths: List[str]):
    paths_bytes = " ".join(paths).encode("utf-8")
    fhandle.write(b":%s" % (name,))

    if path_bytes:
        fhandle.write(b" %s\n" % (path_bytes,))
    else:
        fhandle.write(b"\n")

def load_test_case(test_case_path: str) -> tuple[Optional[TestCase], Optional[str]]:
    try:
        with open(test_case_path, "rb") as fhandle:
            include_paths = read_paths(fhandle, b"include_paths")
            lib_paths = read_paths(fhandle, b"lib_paths")
            argv = []
            argc = read_int(fhandle, b"argc")

            for i in range(argc):
                argv.append(read_bytes(fhandle, b"arg%d" % i).decode("utf-8"))

            stdin = read_bytes(fhandle, b"stdin")
            stdout = read_bytes(fhandle, b"stdout")
            stderr = read_bytes(fhandle, b"stderr")
            returncode = read_int(fhandle, b"returncode")

            return (TestCase(include_paths, lib_paths, argv, stdin, stdout, stderr, returncode), None)
    except Exception as e:
        return (None, str(e))

def save_test_case(test_case_path: str, include_paths: List[str], lib_paths: List[str], argv: List[str],
                   stdin: bytes, stdout: bytes, stderr: bytes, returncode: int):
    with open(test_case_path, "wb") as fhandle:
        write_paths(fhandle, b"include_paths", include_paths)
        write_paths(fhandle, b"lib_paths", lib_paths)
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

def get_test_case_compiler_args(test_case: TestCase, test_file_dir: str, nibble_additional_args: List[str]) -> List[str]:
    compiler_args = []

    if nibble_additional_args:
        compiler_args.extend(nibble_additional_args)

    if test_case:
        # Add program-specific include paths.
        for fp in test_case.include_paths:
            full_path = path.realpath(path.join(test_file_dir, fp))
            compiler_args.extend(["-I", full_path])

        # Add program-specific library paths.
        for fp in test_case.lib_paths:
            full_path = path.realpath(path.join(test_file_dir, fp))
            compiler_args.extend(["-L", full_path])

    return compiler_args


def run_file_test(file_path: str, stats: RunStats, nibble_prog: str, nibble_additional_args: List[str]):
    test_case_path = file_path[:-len(NIBBLE_EXT)] + TEST_CASE_EXT
    test_case, err = load_test_case(test_case_path)
    file_dir = path.dirname(path.realpath(file_path))

    if test_case is not None:
        # Compile nibble program first.
        compiler_args = get_test_case_compiler_args(test_case, file_dir, nibble_additional_args)
        cmd_ret = run_cmd([nibble_prog, *compiler_args, file_path], capture_output=True)

        # Run nibble program if it compiled.
        if cmd_ret.returncode == 0:
            saved_cwd = os.getcwd() # Save current working directory.
            prog_exec = path.join(saved_cwd, "out") # Path of the program's executable.

            os.chdir(file_dir) # Change to the file's directory in case it loads any files with relative paths.

            cmd_ret = run_cmd([prog_exec, *test_case.argv], input=test_case.stdin, capture_output=True) # Run test program.

            os.chdir(saved_cwd) # Restore cwd.

        err = check_test_case(cmd_ret, test_case)

        if err:
            stats.test_failed.append((file_path, err))
    else:
        print(f"[WARNING] Failed to load test case file for {file_path}: {err}", file=sys.stderr)
        compile_ret = run_cmd([nibble_prog, *nibble_additional_args, file_path], capture_output=True)

        if compile_ret.returncode != 0:
            stats.compile_failed.append((file_path, compile_ret.stderr.decode("utf-8")))
        else:
            stats.only_compiled.append(file_path)

    stats.num_tests += 1

def run_dir_tests(folder: str, stats: RunStats, nibble_prog: str, nibble_additional_args: List[str]):
    for e in os.scandir(folder):
        if e.is_file() and e.path.endswith(NIBBLE_EXT):
            run_file_test(e.path, stats, nibble_prog, nibble_additional_args)

def update_file_output(file_path: str, nibble_prog: str, nibble_additional_args: List[str]):
    test_case_path = file_path[:-len(NIBBLE_EXT)] + TEST_CASE_EXT
    test_case, err = load_test_case(test_case_path)
    file_dir = path.dirname(path.realpath(file_path))

    if err is not None:
        test_case = DEFAULT_TEST_CASE

    compiler_args = get_test_case_compiler_args(test_case, file_dir, nibble_additional_args)
    compile_ret = run_cmd([nibble_prog, *compiler_args, file_path], capture_output=True)

    print(f"[INFO] Saving output to {test_case_path}")

    if compile_ret.returncode != 0: # Save compiler error
        save_test_case(test_case_path, test_case.include_paths, test_case.lib_paths, test_case.argv,
                       test_case.stdin, compile_ret.stdout, compile_ret.stderr, compile_ret.returncode)
    else:
        saved_cwd = os.getcwd() # Save current working directory.
        prog_exec = path.join(saved_cwd, "out") # Path of the program's executable.

        os.chdir(file_dir) # Change to the file's directory in case it loads any files with relative paths.

        run_ret = run_cmd([prog_exec, *test_case.argv], input=test_case.stdin, capture_output=True)
        os.chdir(saved_cwd) # Restore cwd.
        save_test_case(test_case_path, test_case.include_paths, test_case.lib_paths, test_case.argv,
                       test_case.stdin, run_ret.stdout, run_ret.stderr, run_ret.returncode)

def update_dir_outputs(folder: str, nibble_prog: str, nibble_additional_args: List[str]):
    for e in os.scandir(folder):
        if e.is_file() and e.path.endswith(NIBBLE_EXT):
            update_file_output(e.path, nibble_prog, nibble_additional_args)

def consume_arg(argv: List[str]) -> tuple[Optional[str], List[str]]:
    if not len(argv):
        return (None, argv)

    arg, *new_argv = argv
    return (arg, new_argv)

def main():
    prog_name, argv = consume_arg(sys.argv)
    nibble_prog = "./nibble"
    update_tests = False
    test_target = "./tests/"
    nibble_additional_args = ["-s"]

    # Parse command-line arguments
    while argv:
        arg, argv = consume_arg(argv)

        if arg == "--nibble_prog":
            if not argv:
                print("[ERROR] Must provide location of Nibble compiler executable file.", file=sys.stderr)
                print_usage(prog_name)
                sys.exit(1)

            nibble_prog, argv = consume_arg(argv)
        elif arg == "--update_tests":
            update_tests = True
        elif arg == "--test_target":
            if not argv:
                print("[ERROR] Must provide test directory or file", file=sys.stderr)
                print_usage(prog_name)
                sys.exit(1)

            test_target, argv = consume_arg(argv)
        elif arg == "--help":
            print_usage(prog_name)
            sys.exit(0)
        else:
            nibble_additional_args.append(arg)

    # Validate path for Nibble executable.
    if not path.exists(nibble_prog):
        print(f"[ERROR] Invalid Nibble executable file: {nibble_prog}", file=sys.stderr)
        os.exit(1)
    else:
        nibble_prog = path.realpath(nibble_prog)

    # Validate path for test_target
    if not path.exists(test_target):
        print(f"[ERROR] Invalid test_target filepath: {test_target}", file=sys.stderr)
        os.exit(1)
    else:
        test_target = path.realpath(test_target)

    if not update_tests: # Run test(s)
        stats = RunStats()

        if path.isdir(test_target):
            run_dir_tests(test_target, stats, nibble_prog, nibble_additional_args)
        elif path.isfile(test_target):
            run_file_test(test_target, stats, nibble_prog, nibble_additional_args)
        else:
            print_usage(prog_name)
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

            sys.exit(1)
        else:
            print(f"\n[INFO] All {stats.num_tests} tests passed")
    else: # Update tests
        if path.isdir(test_target):
            update_dir_outputs(test_target, nibble_prog, nibble_additional_args)
        elif path.isfile(test_target):
            update_file_output(test_target, nibble_prog, nibble_additional_args)
        else:
            print_usage(prog_name)
            print(f"[ERROR] Invalid test target '{test_target}'", file=sys.stderr)
            exit(1)


if __name__ == "__main__":
    main()
