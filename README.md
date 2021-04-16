# Nibble
A programming language based on C. The goal is to create a language with much of the expressive power of C and 
- Generics
- Function overloading
- Default/named function parameters
- Defer statement
- Multiple return values
- Module import system

The syntax is based on C and rust, but is very much subject to change.

### Why not just use C++, D, Odin, or another language? 
I really enjoy using C, but I've always wanted a few features typically found in higher-level languages. That said, I don't claim to have any innovative ideas. I'm doing this primarily for fun. I learned many of the techniques employed in this project from Per Vognsen's Bitwise project and Niklaus Wirth's Compiler Construction textbook.

### Status of the project
I've just started. This is not even close to being done. I've written a basic lexer and parser, and am currently working on type checking/resolution. The first usable version of this compiler will feature an unoptimized x64 backend. Afterwards, I may consider adding an LLVM backend (who knows).

### Build/run
- build: `gcc -o nibble src/main.c`
- run: `./nibble`

At the moment this just prints a program's AST. To call this a compiler at this point would be wishful thinking.
