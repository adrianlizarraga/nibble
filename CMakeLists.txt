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
set()

add_executable(nibble src/main.c)
target_include_directories(nibble PUBLIC "${PROJECT_SOURCE_DIR}/src")
