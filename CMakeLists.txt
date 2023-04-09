cmake_minimum_required(VERSION 3.20)
project(nibble)

set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

add_compile_options(-Wall -Wextra -pedantic)

set(NIBBLE_UTIL_SRCS src/allocator.c
	src/cstring.c
	src/path_utils.c
	src/os_utils.c
	src/print_floats.c
	src/print.c
	src/array.c
	src/hash_map.c
	src/stream.c)
set(NIBBLE_LINKER_SRCS src/linker.c)
set(NIBBLE_LEXER_SRCS src/lexer/module.c)
set(NIBBLE_AST_SRCS src/ast/module.c src/ast/print_ast.c)
set(NIBBLE_PARSER_SRCS src/parser/common.c
	src/parser/typespecs.c
	src/parser/exprs.c
	src/parser/stmts.c
	src/parser/decls.c)
set(NIBBLE_RESOLVER_SRCS src/resolver/module.c
	src/resolver/common.c
	src/resolver/typespecs.c
	src/resolver/exprs.c
	src/resolver/stmts.c
	src/resolver/decls.c)
set(NIBBLE_IR_SRCS src/bytecode/module.c
	src/bytecode/vars.c
	src/bytecode/procs.c
	src/bytecode/print_ir.c)
set(NIBBLE_X64_SRCS src/x64_gen/module.c
    src/x64_gen/regs.c
    src/x64_gen/lir.c
    src/x64_gen/convert_ir.c
    src/x64_gen/livevar.c
    src/x64_gen/reg_alloc.c
    src/x64_gen/print_lir.c
    src/x64_gen/elf.c
    src/x64_gen/data.c
    src/x64_gen/text.c
    src/x64_gen/elf_writer.c
    src/x64_gen/lir_to_x64.c
    src/x64_gen/nasm_gen.c)

set(NIBBLE_MAIN_SRCS src/nibble.c src/main.c)

add_executable(nibble ${NIBBLE_UTIL_SRCS}
	${NIBBLE_LINKER_SRCS}
	${NIBBLE_LEXER_SRCS}
	${NIBBLE_AST_SRCS}
	${NIBBLE_PARSER_SRCS}
	${NIBBLE_RESOLVER_SRCS}
	${NIBBLE_IR_SRCS}
	${NIBBLE_X64_SRCS}
	${NIBBLE_MAIN_SRCS})
target_include_directories(nibble PUBLIC "${PROJECT_SOURCE_DIR}/src")
