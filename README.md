**THIS PROGRAMMING LANGUAGE IS A WORK IN PROGRESS. ANYTHING CAN CHANGE AT ANY MOMENT. USE AT YOUR OWN RISK!**
# Nibble

[![Unit Tests](https://github.com/nibble-language/nibble/actions/workflows/test_nibble_programs.yml/badge.svg)](https://github.com/nibble-language/nibble/actions/workflows/test.yml)

This is an experimental programming language based on C. The goal is to create a compiled, statically-typed programming language that adds the following features to C:

- [x] Order-independent declarations
- [x] Module import system (based on javascript ES6)
- [x] Type inference
- [x] Typed enumerations
- [x] Multiple return values (return anonymous structure object aka tuples)
- [x] Type-safe variadic procedures
- [x] Array slices (See [C's Biggest Mistake](https://digitalmars.com/articles/C-biggest-mistake.html))
- [x] Variables initialized to zero by default (use `---` value to leave uninitialized)
- [ ] Defer statement (like go)
- [ ] Default procedure arguments
- [ ] Named procedure arguments
- [ ] Generic procedures (like c++ templates)

Nibble supports the followng operating systems:
- [x] x64 linux
- [ ] x64 windows
- [ ] x86 linux
- [ ] x86 windows
- [ ] arm64 macOS

Refer to the [language reference document](https://github.com/adrianlizarraga/nibble/wiki/Language-reference) for details. It is very much a work in progress.
## Quickstart
### 1. Build the Nibble compiler
The only library required to build the Nibble compiler is the C standard library.
#### Linux
The following terminal command generates an executable called `nibble` in the root project directory.
```console
$ gcc -I./src -O2 -o nibble src/main.c
```

### 2. Install NASM
The Nibble compiler currently generates an assembly file that is assembled with NASM and then linked with your operating system's linker. Therefore, you'll need to install the [NASM assembler](https://nasm.us/) and add it to your system `PATH`. 

Refer to the wiki page on [How to install NASM](https://github.com/adrianlizarraga/nibble/wiki/How-to-install-NASM) for more detailed instructions.

### 3. Compile a nibble program
Here's an example that compiles the ["Hello World" example program](examples/hello_world/main.nib) on linux.

```console
$ ./nibble -o hello_world examples/hello_world/main.nib
[INFO]: Parsing module examples/hello_world/main.nib ...
[INFO]: Resolving/type-checking ...
[INFO]: Generating IR ...
[INFO]: Generating NASM assembly output: hello_world.s ...
[CMD]: nasm -f elf64 hello_world.s -o hello_world.o
[CMD]: ld -o hello_world hello_world.o
$ ./hello_world
Hello, World

```

To compile a Nibble program, the compiler only needs the file (i.e., module) containing your program's `main()` procedure. The compiler can automatically detect any imported or included files. Refer to the [language reference](https://github.com/adrianlizarraga/nibble/wiki/Language-reference#module-importexport-system) to learn more about importing or including other files.

## Command-line options
Run `./nibble -h` for available command-line options.

```console
$ ./nibble -h
Usage: ./nibble [OPTIONS] <input.nib>
OPTIONS:
    -h                              Print this help message
    -s                              Silent mode (no output to stdout)
    -os   [linux | win32 | osx]     Target OS
    -arch [x64 | x86]               Target architecture
    -I    <module_search_path>      Add module (import/include) search path
    -L    <library_search_path>     Add library search path
    -o    <output_file>             Output binary file name. Defaults to `out`
```

## Code examples

### Hello, World

main.nib:
```c
import "std/basic" as std;

proc main(argc : int, argv : ^^char) => int
{
    std::print_out("Hello, World!\n");

    return 0;
}
```

```console
$ ./nibble -s main.nib -o hello
$ ./hello
Hello, World!

```

### Snake game
This [example game](examples/snake_game/) uses the terminal to display its UI.

```console
$ ./nibble -s examples/snake_game/main.nib -o snake
$ ./snake

```
![](examples/snake_game/snake-game.gif)

## Status of the project

Nibble does not yet support all basic C features:
- [x] Integer types
- [x] Floating-point types
- [x] Structure types
- [x] Union types
- [x] Enum types
- [x] Typedefs
- [x] Procedures
    - [x] Basic procedures with non-variadic parameters
    - [x] Varidic parameters
- [ ] Statements
    - [x] static_assert 
    - [x] if/else
    - [x] while 
    - [x] do while
    - [x] for loop
    - [ ] switch
    - [x] break
    - [x] continue
    - [x] return 
    - [x] Expressions
    - [x] Assignments
- [x] Ternary operator
- [x] Binary operators
- [x] Unary operators

## Language reference
[Here it is](https://github.com/adrianlizarraga/nibble/wiki/Language-reference). It is very much a work in progress.
