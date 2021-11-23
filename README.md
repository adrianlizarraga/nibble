# Nibble
A programming language based on C. The goal is to create a compiled, statically-typed programming language that adds the following features to C:

- [x] Order-independent declarations
- [x] Module import system (based on javascript ES6)
- [x] Type inference
- [ ] Multiple return values (return anonymous structure object)
- [ ] Defer statement (like go)
- [ ] Default procedure arguments
- [ ] Named procedure arguments
- [ ] Generic procedures (like c++ templates)

The Nibble compiler currently generates an assembly file that is assembled with NASM and then linked with the OS linker.

Nibble supports the followng operating systems:
- [x] x64 linux
- [x] x64 windows

## Examples
Note that Nibble provides a builtin procedure called `#writeout()` that writes bytes to stdout.

Hello, World:
```c
proc main(argc : int, argv : ^^char) => int
{
    var msg : []char = "Hello, World\n";
    var len : usize = #sizeof(#typeof(msg)) - 1;

    #writeout(msg, len);
    return 0;
}
```

Merge sort:
```c
proc min(a: int, b: int) => int {
    var r := a;

    if (b < a) {
        r = b;
    }

    return r;
}

proc merge(a: ^int, l: int, m: int, r: int, b: ^int) {
    var i := l;
    var j := m;

    var k := l;

    while (k < r) {
        if (i < m && (j >= r || a[i] <= a[j])) {
            b[k] = a[i];
            i = i + 1;
        }
        else {
            b[k] = a[j];
            j = j + 1;
        }

        k = k + 1;
    }
}

proc merge_sort(a: ^int, b: ^int, n : int) => ^int {
    var width := 1;

    while (width < n) {
        var d := width * 2;
        var i := 0;

        while (i < n) {
            var m := min(i + width, n);
            var r := min(i + d, n);

            merge(a, i, m, r, b);

            i = i + d;
        }

        // Swap.
        var t := a;
        a = b;
        b = t;

        width = d;
    }

    return a;
}

proc main() => int {
    var a : [6]int = {5, 4, 3, 2, 1, 0};
    var b : [6]int;

    var c : ^int = merge_sort(a, b, 6);
   
    var i := 1;
    while (i < 6) {
        var ch : char = c[i] + '0';

        #writeout(^ch, 1);
        #writeout(" ", 1);
    }

    return 0;
}
```

## Quickstart
### Compiling
TODO
### Testing
TODO

## Status of the project

Nibble does not yet support all basic C features:
- [x] Integer types
- [ ] Floating-point types
- [ ] Structure types
- [ ] Union types
- [ ] Enum types
- [ ] Procedures
    - [x] Basic procedures with non-variadic parameters
    - [ ] Varidic parameters
- [ ] Statements
    - [x] if/else
    - [x] while 
    - [x] do while
    - [ ] for loop
    - [ ] switch
    - [ ] break
    - [ ] continue
    - [ ] goto! (yes, I will)
    - [x] return 
    - [x] Expressions
    - [x] Assignments
- [x] Ternary operator
- [x] Binary operators
- [x] Unary operators

