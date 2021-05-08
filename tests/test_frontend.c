//#define NDEBUG 1
#define PRINT_MEM_USAGE 0
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "allocator.c"
#include "array.c"
#include "ast.c"
#include "cstring.c"
#include "hash_map.c"
#include "lexer.c"
#include "llist.h"
#include "types.c"
#include "nibble.c"
#include "parser.c"
#include "print.c"
#include "stream.c"

static void print_errors(ByteStream* errors)
{
    if (errors)
    {
        ByteStreamChunk* chunk = errors->first;

        while (chunk)
        {
            ftprint_out("[ERROR]: %s\n", chunk->buf);
            chunk = chunk->next;
        }
    }
}

#define TKN_TEST_POS(tk, tp, a, b)                                                                                     \
    do                                                                                                                 \
    {                                                                                                                  \
        assert((tk.kind == tp));                                                                                       \
        assert((tk.range.start == a));                                                                                 \
        assert((tk.range.end == b));                                                                                   \
    } while (0)
#define TKN_TEST_INT(tk, b, v, s)                                                                                      \
    do                                                                                                                 \
    {                                                                                                                  \
        assert((tk.kind == TKN_INT));                                                                                  \
        assert((tk.as_int.rep == b));                                                                                  \
        assert((tk.as_int.value == v));                                                                                \
        assert((tk.as_int.suffix == s));                                                                               \
    } while (0)
#define TKN_TEST_FLOAT64(tk, v)                                                                                        \
    do                                                                                                                 \
    {                                                                                                                  \
        assert((tk.kind == TKN_FLOAT));                                                                                \
        assert((tk.as_float.value.f64 == v));                                                                          \
        assert((tk.as_float.value.kind == FLOAT_F64));                                                                 \
    } while (0)
#define TKN_TEST_FLOAT32(tk, v)                                                                                        \
    do                                                                                                                 \
    {                                                                                                                  \
        assert((tk.kind == TKN_FLOAT));                                                                                \
        assert((tk.as_float.value.f32 == v));                                                                          \
        assert((tk.as_float.value.kind == FLOAT_F32));                                                                 \
    } while (0)
#define TKN_TEST_CHAR(tk, v)                                                                                           \
    do                                                                                                                 \
    {                                                                                                                  \
        assert((tk.kind == TKN_INT));                                                                                  \
        assert((tk.as_int.rep == TKN_INT_CHAR));                                                                       \
        assert((tk.as_int.value == v));                                                                                \
    } while (0)
#define TKN_TEST_STR(tk, v)                                                                                            \
    do                                                                                                                 \
    {                                                                                                                  \
        assert((tk.kind == TKN_STR));                                                                                  \
        assert(strcmp(tk.as_str.value, v) == 0);                                                                       \
    } while (0)
#define TKN_TEST_IDEN(tk, v)                                                                                           \
    do                                                                                                                 \
    {                                                                                                                  \
        assert((tk.kind == TKN_IDENT));                                                                                \
        assert(strcmp(tk.as_ident.value, v) == 0);                                                                     \
    } while (0)
#define TKN_TEST_KW(tk, k)                                                                                             \
    do                                                                                                                 \
    {                                                                                                                  \
        assert((tk.kind == TKN_KW));                                                                                   \
        assert(tk.as_kw.kw == (k));                                                                                    \
    } while (0)

static void test_lexer(void)
{
    Allocator allocator = allocator_create(4096);
    Allocator temp = allocator_create(512);

    // Test basic tokens, newlines, and c++ comments.
    {
        unsigned int i = 10;
        Token token = {0};
        Lexer lexer = lexer_create(
            "(+[]-*%&|^<>#) && || => :> >> << == >= <= = += -= *= /= &= |= ^= %= \n  //++--\n{;:,./}", i, &temp, NULL);

        token = scan_token(&lexer);
        TKN_TEST_POS(token, TKN_LPAREN, i, ++i);

        token = scan_token(&lexer);
        TKN_TEST_POS(token, TKN_PLUS, i, ++i);

        token = scan_token(&lexer);
        TKN_TEST_POS(token, TKN_LBRACKET, i, ++i);

        token = scan_token(&lexer);
        TKN_TEST_POS(token, TKN_RBRACKET, i, ++i);

        token = scan_token(&lexer);
        TKN_TEST_POS(token, TKN_MINUS, i, ++i);

        token = scan_token(&lexer);
        TKN_TEST_POS(token, TKN_ASTERISK, i, ++i);

        token = scan_token(&lexer);
        TKN_TEST_POS(token, TKN_MOD, i, ++i);

        token = scan_token(&lexer);
        TKN_TEST_POS(token, TKN_AND, i, ++i);

        token = scan_token(&lexer);
        TKN_TEST_POS(token, TKN_OR, i, ++i);

        token = scan_token(&lexer);
        TKN_TEST_POS(token, TKN_CARET, i, ++i);

        token = scan_token(&lexer);
        TKN_TEST_POS(token, TKN_LT, i, ++i);

        token = scan_token(&lexer);
        TKN_TEST_POS(token, TKN_GT, i, ++i);

        token = scan_token(&lexer);
        TKN_TEST_POS(token, TKN_POUND, i, ++i);

        token = scan_token(&lexer);
        TKN_TEST_POS(token, TKN_RPAREN, i, ++i);

        i++;
        token = scan_token(&lexer);
        TKN_TEST_POS(token, TKN_LOGIC_AND, i, i + 2);
        i += 3;

        token = scan_token(&lexer);
        TKN_TEST_POS(token, TKN_LOGIC_OR, i, i + 2);
        i += 3;

        token = scan_token(&lexer);
        TKN_TEST_POS(token, TKN_ARROW, i, i + 2);
        i += 3;

        token = scan_token(&lexer);
        TKN_TEST_POS(token, TKN_CAST, i, i + 2);
        i += 3;

        token = scan_token(&lexer);
        TKN_TEST_POS(token, TKN_RSHIFT, i, i + 2);
        i += 3;

        token = scan_token(&lexer);
        TKN_TEST_POS(token, TKN_LSHIFT, i, i + 2);
        i += 3;

        token = scan_token(&lexer);
        TKN_TEST_POS(token, TKN_EQ, i, i + 2);
        i += 3;

        token = scan_token(&lexer);
        TKN_TEST_POS(token, TKN_GTEQ, i, i + 2);
        i += 3;

        token = scan_token(&lexer);
        TKN_TEST_POS(token, TKN_LTEQ, i, i + 2);
        i += 3;

        token = scan_token(&lexer);
        TKN_TEST_POS(token, TKN_ASSIGN, i, i + 1);
        i += 2;

        token = scan_token(&lexer);
        TKN_TEST_POS(token, TKN_ADD_ASSIGN, i, i + 2);
        i += 3;

        token = scan_token(&lexer);
        TKN_TEST_POS(token, TKN_SUB_ASSIGN, i, i + 2);
        i += 3;

        token = scan_token(&lexer);
        TKN_TEST_POS(token, TKN_MUL_ASSIGN, i, i + 2);
        i += 3;

        token = scan_token(&lexer);
        TKN_TEST_POS(token, TKN_DIV_ASSIGN, i, i + 2);
        i += 3;

        token = scan_token(&lexer);
        TKN_TEST_POS(token, TKN_AND_ASSIGN, i, i + 2);
        i += 3;

        token = scan_token(&lexer);
        TKN_TEST_POS(token, TKN_OR_ASSIGN, i, i + 2);
        i += 3;

        token = scan_token(&lexer);
        TKN_TEST_POS(token, TKN_XOR_ASSIGN, i, i + 2);
        i += 3;

        token = scan_token(&lexer);
        TKN_TEST_POS(token, TKN_MOD_ASSIGN, i, i + 2);
        i += 3;

        i += 10;
        token = scan_token(&lexer);
        TKN_TEST_POS(token, TKN_LBRACE, i, ++i);

        token = scan_token(&lexer);
        TKN_TEST_POS(token, TKN_SEMICOLON, i, ++i);

        token = scan_token(&lexer);
        TKN_TEST_POS(token, TKN_COLON, i, ++i);

        token = scan_token(&lexer);
        TKN_TEST_POS(token, TKN_COMMA, i, ++i);

        token = scan_token(&lexer);
        TKN_TEST_POS(token, TKN_DOT, i, ++i);

        token = scan_token(&lexer);
        TKN_TEST_POS(token, TKN_DIV, i, ++i);

        token = scan_token(&lexer);
        TKN_TEST_POS(token, TKN_RBRACE, i, ++i);

        token = scan_token(&lexer);
        TKN_TEST_POS(token, TKN_EOF, i, i);
    }

    // Test nested c-style comments
    {
        Lexer lexer = lexer_create("/**** 1 /* 2 */ \n***/+-", 0, &temp, NULL);
        Token token = {0};

        token = scan_token(&lexer);
        TKN_TEST_POS(token, TKN_PLUS, 21, 22);

        token = scan_token(&lexer);
        TKN_TEST_POS(token, TKN_MINUS, 22, 23);

        token = scan_token(&lexer);
        TKN_TEST_POS(token, TKN_EOF, 23, 23);
    }

    // Test error when have unclosed c-style comments
    {
        ByteStream errors = byte_stream_create(&allocator);
        Lexer lexer = lexer_create("/* An unclosed comment", 0, &temp, &errors);
        Token token = {0};

        token = scan_token(&lexer);
        TKN_TEST_POS(token, TKN_EOF, 22, 22);
        assert(errors.count == 1);

        print_errors(&errors);
    }

    {
        ByteStream errors = byte_stream_create(&allocator);
        Lexer lexer = lexer_create("/* An unclosed comment\n", 0, &temp, &errors);
        Token token = {0};

        token = scan_token(&lexer);
        size_t len = cstr_len(lexer.str);
        TKN_TEST_POS(token, TKN_EOF, len, len);
        assert(errors.count == 1);

        print_errors(&errors);
    }

    // Test integer literals
    {
        Lexer lexer = lexer_create("123 333\n0xFF 0b0111 011 0 1u 1ul 1ull 1ULL 2l 2ll 2LL 3lU 3LLU", 0, &temp, NULL);
        Token token = {0};

        token = scan_token(&lexer);
        TKN_TEST_INT(token, TKN_INT_DEC, 123, TKN_INT_SUFFIX_NONE);

        token = scan_token(&lexer);
        TKN_TEST_INT(token, TKN_INT_DEC, 333, TKN_INT_SUFFIX_NONE);

        token = scan_token(&lexer);
        TKN_TEST_INT(token, TKN_INT_HEX, 0xFF, TKN_INT_SUFFIX_NONE);

        token = scan_token(&lexer);
        TKN_TEST_INT(token, TKN_INT_BIN, 7, TKN_INT_SUFFIX_NONE);

        token = scan_token(&lexer);
        TKN_TEST_INT(token, TKN_INT_OCT, 9, TKN_INT_SUFFIX_NONE);

        token = scan_token(&lexer);
        TKN_TEST_INT(token, TKN_INT_DEC, 0, TKN_INT_SUFFIX_NONE);

        token = scan_token(&lexer);
        TKN_TEST_INT(token, TKN_INT_DEC, 1, TKN_INT_SUFFIX_U);

        token = scan_token(&lexer);
        TKN_TEST_INT(token, TKN_INT_DEC, 1, TKN_INT_SUFFIX_UL);

        token = scan_token(&lexer);
        TKN_TEST_INT(token, TKN_INT_DEC, 1, TKN_INT_SUFFIX_ULL);

        token = scan_token(&lexer);
        TKN_TEST_INT(token, TKN_INT_DEC, 1, TKN_INT_SUFFIX_ULL);

        token = scan_token(&lexer);
        TKN_TEST_INT(token, TKN_INT_DEC, 2, TKN_INT_SUFFIX_L);

        token = scan_token(&lexer);
        TKN_TEST_INT(token, TKN_INT_DEC, 2, TKN_INT_SUFFIX_LL);

        token = scan_token(&lexer);
        TKN_TEST_INT(token, TKN_INT_DEC, 2, TKN_INT_SUFFIX_LL);

        token = scan_token(&lexer);
        TKN_TEST_INT(token, TKN_INT_DEC, 3, TKN_INT_SUFFIX_UL);

        token = scan_token(&lexer);
        TKN_TEST_INT(token, TKN_INT_DEC, 3, TKN_INT_SUFFIX_ULL);

        token = scan_token(&lexer);
        assert(token.kind == TKN_EOF);
    }

    // Test integer literal lexing errors
    {
        ByteStream errors = byte_stream_create(&allocator);
        Lexer lexer = lexer_create("0Z 0b3 09 1A\n999999999999999999999999 12ulu", 0, &temp, &errors);
        Token token = {0};

        token = scan_token(&lexer);
        assert(errors.count == 1);

        token = scan_token(&lexer);
        assert(errors.count == 2);

        token = scan_token(&lexer);
        assert(errors.count == 3);

        token = scan_token(&lexer);
        assert(errors.count == 4);

        token = scan_token(&lexer);
        assert(errors.count == 5);

        token = scan_token(&lexer);
        assert(errors.count == 6);

        token = scan_token(&lexer);
        assert(token.kind == TKN_EOF);

        print_errors(&errors);
    }

    // Test floating point literals
    {
        Lexer lexer = lexer_create("1.23 .23 1.33E2 0.1f 0.1F", 0, &temp, NULL);
        Token token = {0};

        token = scan_token(&lexer);
        TKN_TEST_FLOAT64(token, 1.23);

        token = scan_token(&lexer);
        TKN_TEST_FLOAT64(token, .23);

        token = scan_token(&lexer);
        TKN_TEST_FLOAT64(token, 1.33E2);

        token = scan_token(&lexer);
        TKN_TEST_FLOAT32(token, 0.1f);

        token = scan_token(&lexer);
        TKN_TEST_FLOAT32(token, 0.1f);

        token = scan_token(&lexer);
        assert(token.kind == TKN_EOF);
    }

    {
        ByteStream errors = byte_stream_create(&allocator);
        Lexer lexer = lexer_create("1.33ea 1.33e100000000000 1.33e100000000000f", 0, &temp, &errors);

        scan_token(&lexer);
        assert(errors.count == 1);

        scan_token(&lexer);
        assert(errors.count == 2);

        scan_token(&lexer);
        assert(errors.count == 3);

        print_errors(&errors);
    }

    // Test character literals
    {
        ByteStream errors = byte_stream_create(&allocator);
        Lexer lexer = lexer_create("'a' '1' ' ' '\\0' '\\a' '\\b' '\\f' '\\n' '\\r' '\\t' '\\v' "
                                   "'\\\\' '\\'' '\\\"' '\\?'",
                                   0, &temp, &errors);
        Token token = {0};

        token = scan_token(&lexer);
        TKN_TEST_CHAR(token, 'a');

        token = scan_token(&lexer);
        TKN_TEST_CHAR(token, '1');

        token = scan_token(&lexer);
        TKN_TEST_CHAR(token, ' ');

        token = scan_token(&lexer);
        TKN_TEST_CHAR(token, '\0');

        token = scan_token(&lexer);
        TKN_TEST_CHAR(token, '\a');

        token = scan_token(&lexer);
        TKN_TEST_CHAR(token, '\b');

        token = scan_token(&lexer);
        TKN_TEST_CHAR(token, '\f');

        token = scan_token(&lexer);
        TKN_TEST_CHAR(token, '\n');

        token = scan_token(&lexer);
        TKN_TEST_CHAR(token, '\r');

        token = scan_token(&lexer);
        TKN_TEST_CHAR(token, '\t');

        token = scan_token(&lexer);
        TKN_TEST_CHAR(token, '\v');

        token = scan_token(&lexer);
        TKN_TEST_CHAR(token, '\\');

        token = scan_token(&lexer);
        TKN_TEST_CHAR(token, '\'');

        token = scan_token(&lexer);
        TKN_TEST_CHAR(token, '"');

        token = scan_token(&lexer);
        TKN_TEST_CHAR(token, '?');

        token = scan_token(&lexer);
        assert(token.kind == TKN_EOF);
        assert(errors.count == 0);
    }

    // Test escaped hex chars
    {
        ByteStream errors = byte_stream_create(&allocator);
        Lexer lexer = lexer_create("'\\x12'  '\\x3'", 0, &temp, &errors);
        Token token = {0};

        token = scan_token(&lexer);
        TKN_TEST_CHAR(token, '\x12');

        token = scan_token(&lexer);
        TKN_TEST_CHAR(token, '\x3');

        token = scan_token(&lexer);
        assert(token.kind == TKN_EOF);
        assert(errors.count == 0);
    }

    // Test errors when lexing escaped hex chars
    {
        ByteStream errors = byte_stream_create(&allocator);
        Lexer lexer = lexer_create("'' 'a '\n' '\\z' '\\0'", 0, &temp, &errors);
        Token token = {0};

        token = scan_token(&lexer);
        assert(errors.count == 1);

        token = scan_token(&lexer);
        assert(errors.count == 2);

        token = scan_token(&lexer);
        assert(errors.count == 3);

        token = scan_token(&lexer);
        assert(errors.count == 4);

        token = scan_token(&lexer);
        TKN_TEST_CHAR(token, '\0');

        token = scan_token(&lexer);
        assert(token.kind == TKN_EOF);

        print_errors(&errors);
    }

    // Test basic string literals
    {
        ByteStream errors = byte_stream_create(&allocator);
        const char* str = "\"hello world\" \"a\\nb\" \n \"\\x50 a \\x51\" \"\" \"\\\"nested\\\"\"";
        Lexer lexer = lexer_create(str, 0, &temp, &errors);
        Token token = {0};

        token = scan_token(&lexer);
        TKN_TEST_STR(token, "hello world");

        token = scan_token(&lexer);
        TKN_TEST_STR(token, "a\nb");

        token = scan_token(&lexer);
        TKN_TEST_STR(token, "\x50 a \x51");

        token = scan_token(&lexer);
        TKN_TEST_STR(token, "");

        token = scan_token(&lexer);
        TKN_TEST_STR(token, "\"nested\"");

        token = scan_token(&lexer);
        assert(token.kind == TKN_EOF);
        assert(errors.count == 0);
    }

    // Test errors when scanning string literals
    {
        ByteStream errors = byte_stream_create(&allocator);
        const char* str = "\"\n\" \"\\xTF\" \"\\W\" \"unclosed";
        Lexer lexer = lexer_create(str, 0, &temp, &errors);

        scan_token(&lexer);
        assert(errors.count == 1);

        scan_token(&lexer);
        assert(errors.count == 2);

        scan_token(&lexer);
        assert(errors.count == 3);

        scan_token(&lexer);
        assert(errors.count == 4);

        print_errors(&errors);
    }

    // Test basic identifiers
    {
        ByteStream errors = byte_stream_create(&allocator);
        const char* str = "vars x1a x11 _abc abc_ _ab_c_ i";
        Lexer lexer = lexer_create(str, 0, &temp, &errors);
        Token token = {0};

        token = scan_token(&lexer);
        TKN_TEST_IDEN(token, "vars");

        token = scan_token(&lexer);
        TKN_TEST_IDEN(token, "x1a");

        token = scan_token(&lexer);
        TKN_TEST_IDEN(token, "x11");

        token = scan_token(&lexer);
        TKN_TEST_IDEN(token, "_abc");

        token = scan_token(&lexer);
        TKN_TEST_IDEN(token, "abc_");

        token = scan_token(&lexer);
        TKN_TEST_IDEN(token, "_ab_c_");

        token = scan_token(&lexer);
        TKN_TEST_IDEN(token, "i");

        token = scan_token(&lexer);
        assert(token.kind == TKN_EOF);
        assert(errors.count == 0);
    }

    // Test invalid identifier combinations.
    {
        ByteStream errors = byte_stream_create(&allocator);
        const char* str = "1var";
        Lexer lexer = lexer_create(str, 0, &temp, &errors);
        Token token = {0};

        token = scan_token(&lexer);
        assert(errors.count == 1);

        token = scan_token(&lexer);
        assert(token.kind == TKN_EOF);

        print_errors(&errors);
    }

    // Test keywords
    {
        ByteStream errors = byte_stream_create(&allocator);
        char* str = array_create(&allocator, char, 256);

        for (int i = 0; i < KW_COUNT; i += 1)
        {
            ftprint_char_array(&str, false, "%s ", keyword_names[i].str);
        }

        array_push(str, '\0');

        Lexer lexer = lexer_create(str, 0, &temp, &errors);
        Token token = {0};

        for (int i = 0; i < KW_COUNT; i += 1)
        {
            token = scan_token(&lexer);
            TKN_TEST_KW(token, (Keyword)i);
        }

        token = scan_token(&lexer);
        assert(token.kind == TKN_EOF);
        assert(errors.count == 0);
    }

    allocator_destroy(&allocator);
    allocator_destroy(&temp);
}

static bool test_parse_typespec(Allocator* gen_arena, Allocator* ast_arena, const char* code, const char* sexpr)
{
    ByteStream err_stream = byte_stream_create(gen_arena);
    Parser parser = {0};

    parser_init(&parser, ast_arena, code, 0, &err_stream);
    next_token(&parser);

    TypeSpec* type = parse_typespec(&parser);
    char* s = ftprint_typespec(gen_arena, type);
    AllocatorStats stats = allocator_stats(ast_arena);

    ftprint_out("%s\n\t%s\n\tAST mem size: %lu bytes\n", code, s, stats.used);

    if (err_stream.count)
        print_errors(&err_stream);

    parser_destroy(&parser);
    allocator_reset(ast_arena);
    allocator_reset(gen_arena);

    return cstr_cmp(s, sexpr) == 0;
}

static bool test_parse_expr(Allocator* gen_arena, Allocator* ast_arena, const char* code, const char* sexpr)
{
    ByteStream err_stream = byte_stream_create(gen_arena);
    Parser parser = {0};

    parser_init(&parser, ast_arena, code, 0, &err_stream);
    next_token(&parser);

    Expr* expr = parse_expr(&parser);
    char* s = ftprint_expr(gen_arena, expr);
    AllocatorStats stats = allocator_stats(ast_arena);

    ftprint_out("%s\n\t%s\n\tAST mem size: %lu bytes\n", code, s, stats.used);

    if (err_stream.count)
        print_errors(&err_stream);

    parser_destroy(&parser);
    allocator_reset(ast_arena);
    allocator_reset(gen_arena);

    return cstr_cmp(s, sexpr) == 0;
}

static bool test_parse_decl(Allocator* gen_arena, Allocator* ast_arena, const char* code, const char* sexpr)
{
    ByteStream err_stream = byte_stream_create(gen_arena);
    Parser parser = {0};

    parser_init(&parser, ast_arena, code, 0, &err_stream);
    next_token(&parser);

    Decl* decl = parse_decl(&parser);
    char* s = ftprint_decl(gen_arena, decl);
    AllocatorStats stats = allocator_stats(ast_arena);

    ftprint_out("%s\n\t%s\n\tAST mem size: %lu bytes\n", code, s, stats.used);

    if (err_stream.count)
        print_errors(&err_stream);

    parser_destroy(&parser);
    allocator_reset(ast_arena);
    allocator_reset(gen_arena);

    return cstr_cmp(s, sexpr) == 0;
}

static bool test_parse_stmt(Allocator* gen_arena, Allocator* ast_arena, const char* code, const char* sexpr)
{
    ByteStream err_stream = byte_stream_create(gen_arena);
    Parser parser = {0};

    parser_init(&parser, ast_arena, code, 0, &err_stream);
    next_token(&parser);

    Stmt* stmt = parse_stmt(&parser);
    char* s = ftprint_stmt(gen_arena, stmt);
    AllocatorStats stats = allocator_stats(ast_arena);

    ftprint_out("%s\n\t%s\n\tAST mem size: %lu bytes\n", code, s, stats.used);

    if (err_stream.count)
        print_errors(&err_stream);

    parser_destroy(&parser);
    allocator_reset(ast_arena);
    allocator_reset(gen_arena);

    return cstr_cmp(s, sexpr) == 0;
}

void test_parser(void)
{
    Allocator ast_arena = allocator_create(4096);
    Allocator gen_arena = allocator_create(4096);

#define TEST_TYPESPEC(c, s) assert(test_parse_typespec(&gen_arena, &ast_arena, (c), (s)))
#define TEST_EXPR(c, s) assert(test_parse_expr(&gen_arena, &ast_arena, (c), (s)))
#define TEST_DECL(c, s) assert(test_parse_decl(&gen_arena, &ast_arena, (c), (s)))
#define TEST_STMT(c, s) assert(test_parse_stmt(&gen_arena, &ast_arena, (c), (s)))

    // Test base typespecs
    TEST_TYPESPEC("int32", "(:ident int32)");
    TEST_TYPESPEC("(int32)", "(:ident int32)");
    TEST_TYPESPEC("proc(int32, b:float32) => float32",
                  "(:proc =>(:ident float32) (:ident int32) (b (:ident float32)))");
    TEST_TYPESPEC("proc(int32, b:float32)", "(:proc => (:ident int32) (b (:ident float32)))");
    TEST_TYPESPEC("struct {a:int32; b:float32;}", "(:struct (a (:ident int32)) (b (:ident float32)))");
    TEST_TYPESPEC("union {a:int32; b:float32;}", "(:union (a (:ident int32)) (b (:ident float32)))");
    TEST_TYPESPEC("Lexer.Token", "(:ident Lexer.Token)");
    TEST_TYPESPEC("Parser.Lexer.Token", "(:ident Parser.Lexer.Token)");

    // Test pointer to base types
    TEST_TYPESPEC("^int32", "(:ptr (:ident int32))");
    TEST_TYPESPEC("^(int32)", "(:ptr (:ident int32))");
    TEST_TYPESPEC("^proc(int32, b:float32) => float32",
                  "(:ptr (:proc =>(:ident float32) (:ident int32) (b (:ident float32))))");
    TEST_TYPESPEC("^struct {a:int32; b:float32;}", "(:ptr (:struct (a (:ident int32)) (b (:ident float32))))");
    TEST_TYPESPEC("^union {a:int32; b:float32;}", "(:ptr (:union (a (:ident int32)) (b (:ident float32))))");

    // Test array of base types
    TEST_TYPESPEC("[3]int32", "(:arr 3 (:ident int32))");
    TEST_TYPESPEC("[](int32)", "(:arr (:ident int32))");
    TEST_TYPESPEC("[3]proc(int32, b:float32) => float32",
                  "(:arr 3 (:proc =>(:ident float32) (:ident int32) (b (:ident float32))))");
    TEST_TYPESPEC("[3]struct {a:int32; b:float32;}", "(:arr 3 (:struct (a (:ident int32)) (b (:ident float32))))");
    TEST_TYPESPEC("[3]union {a:int32; b:float32;}", "(:arr 3 (:union (a (:ident int32)) (b (:ident float32))))");

    // Test const base types
    TEST_TYPESPEC("const int32", "(:const (:ident int32))");
    TEST_TYPESPEC("const (int32)", "(:const (:ident int32))");
    TEST_TYPESPEC("const proc(int32, b:float32) => float32",
                  "(:const (:proc =>(:ident float32) (:ident int32) (b (:ident float32))))");
    TEST_TYPESPEC("const struct {a:int32; b:float32;}", "(:const (:struct (a (:ident int32)) (b (:ident float32))))");
    TEST_TYPESPEC("const union {a:int32; b:float32;}", "(:const (:union (a (:ident int32)) (b (:ident float32))))");

    // Test mix of nested type modifiers
    TEST_TYPESPEC("[3]^ const char", "(:arr 3 (:ptr (:const (:ident char))))");

    // Test base expressions
    TEST_EXPR("1", "1");
    TEST_EXPR("3.14", "3.140000");
    TEST_EXPR("\"hi\"", "\"hi\"");
    TEST_EXPR("x", "x");
    TEST_EXPR("(x)", "x");
    TEST_EXPR("{0, 1}", "(compound {0 1})");
    TEST_EXPR("{0, 1 :Vec2i}", "(compound (:ident Vec2i) {0 1})");
    TEST_EXPR("{[0] = 0, [1] = 1}", "(compound {[0] = 0 [1] = 1})");
    TEST_EXPR("{x = 0, y = 1}", "(compound {x = 0 y = 1})");
    TEST_EXPR("sizeof(int32)", "(sizeof (:ident int32))");
    TEST_EXPR("typeof(x)", "(typeof x)");
    TEST_EXPR("(typeof(x))", "(typeof x)");

    // Test expression modifiers
    TEST_EXPR("x.y", "(field x y)");
    TEST_EXPR("x.y.z", "(field (field x y) z)");
    TEST_EXPR("x[0]", "(index x 0)");
    TEST_EXPR("x[0][1]", "(index (index x 0) 1)");
    TEST_EXPR("f(1)", "(call f 1)");
    TEST_EXPR("f(1, y=2)", "(call f 1 y=2)");
    TEST_EXPR("-x:>int", "(- (cast (:ident int) x))");
    TEST_EXPR("(-x):>int", "(cast (:ident int) (- x))");

    // Test unary + base expressions
    TEST_EXPR("-+~!p.a", "(- (+ (~ (! (field p a)))))");
    TEST_EXPR("*^p.a", "(* (^ (field p a)))");

    // Test expressions with multiplicative and unary precedence
    TEST_EXPR("a * *ptr / -b", "(/ (* a (* ptr)) (- b))");
    TEST_EXPR("a % b & !c", "(& (% a b) (! c))");
    TEST_EXPR("a << *p >> c", "(>> (<< a (* p)) c)");

    // Test expressions with additive and multiplicative precedence
    TEST_EXPR("a * b + c / d - e % f | g & h ^ x & y", "(^ (| (- (+ (* a b) (/ c d)) (% e f)) (& g h)) (& x y))");

    // Test expressions with comparative and additive precedence
    TEST_EXPR("a + b == -c - d", "(== (+ a b) (- (- c) d))");
    TEST_EXPR("a + b != -c - d", "(!= (+ a b) (- (- c) d))");
    TEST_EXPR("a + b > -c - d", "(> (+ a b) (- (- c) d))");
    TEST_EXPR("a + b >= -c - d", "(>= (+ a b) (- (- c) d))");
    TEST_EXPR("a + b < -c - d", "(< (+ a b) (- (- c) d))");
    TEST_EXPR("a + b <= -c - d", "(<= (+ a b) (- (- c) d))");

    // Test expressions with logical "or", logical "and", and comparative precedence
    TEST_EXPR("a == b && c > d", "(&& (== a b) (> c d))");
    TEST_EXPR("a && b || c == d", "(|| (&& a b) (== c d))");

    // Large ternary expression
    TEST_EXPR("x > 3 ? -2*x : f(1,b=2) - (3.14 + y.val) / z[2]",
              "(? (> x 3) (* (- 2) x) (- (call f 1 b=2) (/ (+ 3.140000 (field y val)) (index z 2))))");

    // Other expressions to test for the hell of it
    TEST_EXPR("a ^ ^b", "(^ a (^ b))");
    TEST_EXPR("\"abc\"[0]", "(index \"abc\" 0)");
    TEST_EXPR("(a :> (int)) + 2", "(+ (cast (:ident int) a) 2)");
    TEST_EXPR("(a:>^int)", "(cast (:ptr (:ident int)) a)");
    TEST_EXPR("a:>^int", "(cast (:ptr (:ident int)) a)");

    // Test variable declarations
    TEST_DECL("var a : int;", "(var a (:ident int))");
    TEST_DECL("var a := 1;", "(var a 1)");
    TEST_DECL("var a : int = 1;", "(var a (:ident int) 1)");

    // Test const declarations
    TEST_DECL("const a := 1;", "(const a 1)");
    TEST_DECL("const a : int = 1;", "(const a (:ident int) 1)");

    // Test enum declarations
    TEST_DECL("enum Kind {A}", "(enum Kind A)");
    TEST_DECL("enum Kind {A, B,}", "(enum Kind A B)");
    TEST_DECL("enum Kind {A = 0, B, C = 2}", "(enum Kind A=0 B C=2)");
    TEST_DECL("enum Kind :int {A}", "(enum Kind (:ident int) A)");

    // Test struct declarations
    TEST_DECL("struct A {x: int;}", "(struct A (x (:ident int)))");
    TEST_DECL("struct A {x: int; y: float32;}", "(struct A (x (:ident int)) (y (:ident float32)))");
    TEST_DECL("struct A {f: union {a:float32; b:float64;};}",
              "(struct A (f (:union (a (:ident float32)) (b (:ident float64)))))");
    TEST_DECL("struct A {f: struct {a:float32; b:float64;};}",
              "(struct A (f (:struct (a (:ident float32)) (b (:ident float64)))))");

    // Test union declarations
    TEST_DECL("union A {x: int;}", "(union A (x (:ident int)))");
    TEST_DECL("union A {x: int; y: float32;}", "(union A (x (:ident int)) (y (:ident float32)))");
    TEST_DECL("union A {f: union {a:float32; b:float64;};}",
              "(union A (f (:union (a (:ident float32)) (b (:ident float64)))))");
    TEST_DECL("union A {f: struct {a:float32; b:float64;};}",
              "(union A (f (:struct (a (:ident float32)) (b (:ident float64)))))");

    // Test typedef declarations
    TEST_DECL("typedef i32 = int;", "(typedef i32 (:ident int))");

    // Test proc declarations
    TEST_DECL("proc f(){}", "(proc f () => (stmt-block))");
    TEST_DECL("proc f(a:int) => void {}", "(proc f ((a (:ident int))) =>(:ident void) (stmt-block))");
    TEST_DECL("proc f(a:int, b:int) => int {}",
              "(proc f ((a (:ident int)) (b (:ident int))) =>(:ident int) (stmt-block))");
    TEST_DECL("proc f() => int {return 0;}", "(proc f () =>(:ident int) (stmt-block (return 0)))");

    // Test decl statements
    TEST_STMT("var a : int;", "(var a (:ident int))");
    TEST_STMT("const a := 1;", "(const a 1)");
    TEST_STMT("enum Kind {A}", "(enum Kind A)");
    TEST_STMT("struct A {x: int;}", "(struct A (x (:ident int)))");
    TEST_STMT("union A {x: int; y: float32;}", "(union A (x (:ident int)) (y (:ident float32)))");
    TEST_STMT("typedef i32 = int;", "(typedef i32 (:ident int))");
    TEST_STMT("proc f(){}", "(proc f () => (stmt-block))");

    // Test expression statements
    TEST_STMT("f(1);", "(call f 1)");
    TEST_STMT("f(1, b=3);", "(call f 1 b=3)");
    TEST_STMT("*p = 3;", "(= (* p) 3)");
    TEST_STMT("a = 3;", "(= a 3)");
    TEST_STMT("a += 3;", "(+= a 3)");
    TEST_STMT("a -= 3;", "(-= a 3)");
    TEST_STMT("a *= 3;", "(*= a 3)");
    TEST_STMT("a /= 3;", "(/= a 3)");
    TEST_STMT("a &= 3;", "(&= a 3)");
    TEST_STMT("a |= 3;", "(|= a 3)");
    TEST_STMT("a ^= 3;", "(^= a 3)");
    TEST_STMT("a %= 3;", "(%= a 3)");

    // Test no-op, return, break, continue, goto, label statements
    TEST_STMT(";", "no-op");
    TEST_STMT("return 0;", "(return 0)");
    TEST_STMT("break;", "(break)");
    TEST_STMT("break loop1;", "(break loop1)");
    TEST_STMT("continue;", "(continue)");
    TEST_STMT("continue loop1;", "(continue loop1)");
    TEST_STMT("goto top;", "(goto top)");
    TEST_STMT("label top:;", "(label top no-op)");
    TEST_STMT("label top: g = 1;", "(label top (= g 1))");

    // Test if statements
    TEST_STMT("if(a==2){}", "(if (== a 2) (stmt-block))");
    TEST_STMT("if(a==2);", "(if (== a 2) no-op)");
    TEST_STMT("if(a==2){g=2;}", "(if (== a 2) (stmt-block (= g 2)))");
    TEST_STMT("if(a==2){g=2;} else {g=3;}", "(if (== a 2) (stmt-block (= g 2)) (else (stmt-block (= g 3))))");
    TEST_STMT("if(a==2){g=2;} elif(a >= 10) {g = 1;} else {g=3;}",
              "(if (== a 2) (stmt-block (= g 2)) (elif (>= a 10) (stmt-block (= g 1))) (else (stmt-block (= g 3))))");

    // Test while loop statements
    TEST_STMT("while(a!=0){}", "(while (!= a 0) (stmt-block))");
    TEST_STMT("while(a!=0);", "(while (!= a 0) no-op)");
    TEST_STMT("while(a!=0){g = 1;}", "(while (!= a 0) (stmt-block (= g 1)))");

    // Test do-while loop statements
    TEST_STMT("do{}while(a!=0);", "(do-while (!= a 0) (stmt-block))");
    TEST_STMT("do;while(a!=0);", "(do-while (!= a 0) no-op)");
    TEST_STMT("do{g = 1;}while(a!=0);", "(do-while (!= a 0) (stmt-block (= g 1)))");

    // Test for-loop statements
    TEST_STMT("for(;;);", "(for ; ;  no-op)");
    TEST_STMT("for(;;){}", "(for ; ;  (stmt-block))");
    TEST_STMT("for(;;){g += 1;}", "(for ; ;  (stmt-block (+= g 1)))");
    TEST_STMT("for(var i:=0;;);", "(for (var i 0); ;  no-op)");
    TEST_STMT("for(i=0;;);", "(for (= i 0); ;  no-op)");
    TEST_STMT("for(;i < 10;);", "(for ; (< i 10);  no-op)");
    TEST_STMT("for(;; i += 1);", "(for ; ; (+= i 1) no-op)");
    TEST_STMT("for(i=0;i<10;i+=1);", "(for (= i 0); (< i 10); (+= i 1) no-op)");

    // Test switch-statements
    TEST_STMT("switch (a) {case 0: break;}", "(switch a (case 0 (stmt-list (break))))");
    TEST_STMT("switch (a) {case 0: case 1: break;}", "(switch a (case 0 (stmt-list)) (case 1 (stmt-list (break))))");
    TEST_STMT("switch (a) {case 0 .. 10: break;}", "(switch a (case 0..10 (stmt-list (break))))");
    TEST_STMT(
        "switch (a) {case 0: break; case 1: g = 1; break; case: g = 0;}",
        "(switch a (case 0 (stmt-list (break))) (case 1 (stmt-list (= g 1) (break))) (case (stmt-list (= g 0))))");

    // Test block statements.
    TEST_STMT("{}", "(stmt-block)");
    TEST_STMT("{;}", "(stmt-block no-op)");
    TEST_STMT("{g = 10;}", "(stmt-block (= g 10))");
    TEST_STMT("{for(;;);}", "(stmt-block (for ; ;  no-op))");

#undef TEST_TYPESPEC
#undef TEST_EXPR
#undef TEST_DECL
#undef TEST_STMT

    allocator_destroy(&gen_arena);
    allocator_destroy(&ast_arena);
}

int main(void)
{
    ftprint_out("Nibble tests!\n");

    // TODO: Retrieve from command-line or environment.
    OS target_os = OS_LINUX;
    Arch target_arch = ARCH_X64;

    if (!nibble_init(target_os, target_arch))
    {
        ftprint_err("Failed to initialize Nibble\n");
        exit(1);
    }

    test_lexer();
    test_parser();

    nibble_cleanup();
}

