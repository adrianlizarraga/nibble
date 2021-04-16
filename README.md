# Nibble
A programming language based on C. The goal is to create a language with much of the expressive power of C and 
- Generics
- Function overloading
- Default/named function parameters
- Defer statement
- Multiple return values
- Module import system

The syntax is based on C and rust, but is very much subject to change.

Example:
```
struct T {
    a : int32;
}


const MAX_LEN : int32 = 20;

var g : int32 = 10;

proc add_ints(a : int32, b : int32) => int32 {
    return a + b;
}

proc main(argc : int32, argv : [] ^char) => int32
{
    var t : T;
    t.a = 10;

    var b : int32 = 2;
    var c : int32 = add_ints(t.a, b);

    var t2 := {10 : T};
    var t3 : T = {10};
    var t4 : T = {a = 10 : T};

    var d : float32 = 10:>float + 1.0;

    return 0;
}
```

### Why not just use C++, D, Odin, or another language? 
I really enjoy using C, but I've always wanted a few features typically found in higher-level languages. That said, I don't claim to have any innovative ideas. I'm doing this primarily for fun. I learned many of the techniques employed in this project from Per Vognsen's Bitwise project and Niklaus Wirth's Compiler Construction textbook.

### Status of the project
I've just started. This is not even close to being done. I've written a basic lexer and parser, and am currently working on type checking/resolution. The first usable version of this compiler will feature an unoptimized x64 backend. Afterwards, I may consider adding an LLVM backend (who knows).

### Build/run
- build: `gcc -o nibble src/main.c`
- run: `./nibble main.nib`

At the moment this just prints a program's AST. To call this a compiler at this point would be wishful thinking.
