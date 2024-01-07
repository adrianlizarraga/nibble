#ifndef NIBBLE_ALL_SRCS_H
#define NIBBLE_ALL_SRCS_H

// Utils
#include "basics.c"
#include "allocator.c"
#include "cstring.c"
#include "path_utils.c"
#include "os_utils.c"
#include "print_floats.c"
#include "print.c"
#include "array.c"
#include "hash_map.c"
#include "stream.c"

// Linker
#include "linker.c"

// Lexer
#include "lexer/module.c"

// AST
#include "ast/module.c"
#include "ast/print_ast.c"

// Parser
#include "parser/common.c"
#include "parser/typespecs.c"
#include "parser/exprs.c"
#include "parser/stmts.c"
#include "parser/decls.c"

// Resolver/type-checker
#include "resolver/module.c"
#include "resolver/common.c"
#include "resolver/typespecs.c"
#include "resolver/exprs.c"
#include "resolver/stmts.c"
#include "resolver/decls.c"

// Intermediate representation (IR)
#include "bytecode/module.c"
#include "bytecode/vars.c"
#include "bytecode/procs.c"
#include "bytecode/print_ir.c"

// X86_64 backend
#include "x64_gen/regs.c"
#include "x64_gen/xir.c"
#include "x64_gen/ir_to_xir.c"
#include "x64_gen/livevar.c"
#include "x64_gen/reg_alloc.c"
#include "x64_gen/print_xir.c"
#include "x64_gen/elf.c"
#include "x64_gen/data.c"
#include "x64_gen/text.c"
#include "x64_gen/elf_writer.c"
#include "x64_gen/x64_instrs.c"
#include "x64_gen/machine_code.c"
#include "x64_gen/xir_to_x64.c"
#include "x64_gen/nasm_gen.c"

// Compiler main logic
#include "nibble.c"
#include "compiler.c"
#endif
