//#define NDEBUG 1
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "nibble.h"
#include "allocator.c"
#include "array.c"
#include "hash_map.c"
#include "stream.c"
#include "lexer.c"

typedef struct NibbleCtx {
    Allocator allocator;
} NibbleCtx;

static NibbleCtx g_ctx;

static void print_errors(ByteStream* errors)
{
    if (errors) {
        ByteStreamChunk* chunk = errors->first;

        while (chunk) {
            printf("[ERROR]: %s\n", chunk->buf);
            chunk = chunk->next;
        }
    }
}

const char* intern_str_lit(const char* str, size_t len)
{
    char* dup = new_array(&g_ctx.allocator, char, len + 1, false);

    for (size_t i = 0; i < len; ++i) {
        dup[i] = str[i];
    }
    dup[len] = '\0';

    return dup;
}

const char* intern_ident(const char* str, size_t len)
{
    char* dup = new_array(&g_ctx.allocator, char, len + 1, false);

    for (size_t i = 0; i < len; ++i) {
        dup[i] = str[i];
    }
    dup[len] = '\0';

    return dup;
}

#define TKN_TEST_POS(tk, tp, a, b)                                                                                     \
    do {                                                                                                               \
        assert((tk.kind == tp));                                                                                       \
        assert((tk.range.start == a));                                                                                       \
        assert((tk.range.end == b));                                                                                         \
    } while (0)
#define TKN_TEST_INT(tk, b, v)                                                                                         \
    do {                                                                                                               \
        assert((tk.kind == TKN_INT));                                                                                  \
        assert((tk.tint.rep == b));                                                                                    \
        assert((tk.tint.value == v));                                                                                  \
    } while (0)
#define TKN_TEST_FLOAT(tk, v)                                                                                          \
    do {                                                                                                               \
        assert((tk.kind == TKN_FLOAT));                                                                                \
        assert((tk.tfloat.value == v));                                                                                \
    } while (0)
#define TKN_TEST_CHAR(tk, v)                                                                                           \
    do {                                                                                                               \
        assert((tk.kind == TKN_INT));                                                                                  \
        assert((tk.tint.rep == TKN_INT_CHAR));                                                                         \
        assert((tk.tint.value == v));                                                                                  \
    } while (0)
#define TKN_TEST_STR(tk, v)                                                                                            \
    do {                                                                                                               \
        assert((tk.kind == TKN_STR));                                                                                  \
        assert(strcmp(tk.tstr.value, v) == 0);                                                                         \
    } while (0)
#define TKN_TEST_IDEN(tk, v)                                                                                           \
    do {                                                                                                               \
        assert((tk.kind == TKN_IDENT));                                                                           \
        assert(strcmp(tk.tident.value, v) == 0);                                                                  \
    } while (0)

static void test_lexer(void)
{
    // Test basic tokens, newlines, and c++ comments.
    {
        unsigned int i = 10;
        Token token = {0};
        Lexer lexer = lexer_create("(+[]-*%&|^<>) && || >> << == >= <= = += -= *= /= &= |= ^= %= \n  //++--\n{;:,./}", i, NULL);

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
        TKN_TEST_POS(token, TKN_XOR, i, ++i);

        token = scan_token(&lexer);
        TKN_TEST_POS(token, TKN_LT, i, ++i);

        token = scan_token(&lexer);
        TKN_TEST_POS(token, TKN_GT, i, ++i);

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
        TKN_TEST_POS(token, TKN_EOF, i, ++i);

        lexer_destroy(&lexer);
    }

    // Test nested c-style comments
    {
        Lexer lexer = lexer_create("/**** 1 /* 2 */ \n***/+-", 0, NULL);
        Token token = {0};

        token = scan_token(&lexer);
        TKN_TEST_POS(token, TKN_PLUS, 21, 22);

        token = scan_token(&lexer);
        TKN_TEST_POS(token, TKN_MINUS, 22, 23);

        token = scan_token(&lexer);
        TKN_TEST_POS(token, TKN_EOF, 23, 24);

        lexer_destroy(&lexer);
    }

    // Test error when have unclosed c-style comments
    {
        ByteStream errors = byte_stream_create(&g_ctx.allocator);
        Lexer lexer = lexer_create("/* An unclosed comment", 0, &errors);
        Token token = {0};

        token = scan_token(&lexer);
        TKN_TEST_POS(token, TKN_EOF, 22, 23);
        assert(errors.num_chunks == 1);

        print_errors(&errors);

        lexer_destroy(&lexer);
    }

    {
        ByteStream errors = byte_stream_create(&g_ctx.allocator);
        Lexer lexer = lexer_create("/* An unclosed comment\n", 0, &errors);
        Token token = {0};

        token = scan_token(&lexer);
        TKN_TEST_POS(token, TKN_EOF, strlen(lexer.str), strlen(lexer.str) + 1);
        assert(errors.num_chunks == 1);

        print_errors(&errors);
        lexer_destroy(&lexer);
    }

    // Test integer literals
    {
        Lexer lexer = lexer_create("123 333\n0xFF 0b0111 011 0", 0, NULL);
        Token token = {0};

        token = scan_token(&lexer);
        TKN_TEST_INT(token, TKN_INT_DEC, 123);

        token = scan_token(&lexer);
        TKN_TEST_INT(token, TKN_INT_DEC, 333);

        token = scan_token(&lexer);
        TKN_TEST_INT(token, TKN_INT_HEX, 0xFF);

        token = scan_token(&lexer);
        TKN_TEST_INT(token, TKN_INT_BIN, 7);

        token = scan_token(&lexer);
        TKN_TEST_INT(token, TKN_INT_OCT, 9);

        token = scan_token(&lexer);
        TKN_TEST_INT(token, TKN_INT_DEC, 0);

        token = scan_token(&lexer);
        assert(token.kind == TKN_EOF);

        lexer_destroy(&lexer);
    }

    // Test integer literal lexing errors
    {
        ByteStream errors = byte_stream_create(&g_ctx.allocator);
        Lexer lexer = lexer_create("0Z 0b3 09 1A\n999999999999999999999999", 0, &errors);
        Token token = {0};

        token = scan_token(&lexer);
        assert(errors.num_chunks == 1);

        token = scan_token(&lexer);
        assert(errors.num_chunks == 2);

        token = scan_token(&lexer);
        assert(errors.num_chunks == 3);

        token = scan_token(&lexer);
        assert(errors.num_chunks == 4);

        token = scan_token(&lexer);
        assert(errors.num_chunks == 5);

        token = scan_token(&lexer);
        assert(token.kind == TKN_EOF);

        print_errors(&errors);
        lexer_destroy(&lexer);
    }

    // Test floating point literals
    {
        Lexer lexer = lexer_create("1.23 .23 1.33E2", 0, NULL);
        Token token = {0};

        token = scan_token(&lexer);
        TKN_TEST_FLOAT(token, 1.23);

        token = scan_token(&lexer);
        TKN_TEST_FLOAT(token, .23);

        token = scan_token(&lexer);
        TKN_TEST_FLOAT(token, 1.33E2);

        lexer_destroy(&lexer);
    }

    {
        ByteStream errors = byte_stream_create(&g_ctx.allocator);
        Lexer lexer = lexer_create("1.33ea 1.33e100000000000", 0, &errors);

        scan_token(&lexer);
        assert(errors.num_chunks == 1);

        scan_token(&lexer);
        assert(errors.num_chunks == 2);

        print_errors(&errors);
        lexer_destroy(&lexer);
    }

    // Test character literals
    {
        ByteStream errors = byte_stream_create(&g_ctx.allocator);
        Lexer lexer = lexer_create("'a' '1' ' ' '\\0' '\\a' '\\b' '\\f' '\\n' '\\r' '\\t' '\\v' "
                        "'\\\\' '\\'' '\\\"' '\\?'",
                        0, &errors);
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
        assert(errors.num_chunks == 0);

        lexer_destroy(&lexer);
    }

    // Test escaped hex chars
    {
        ByteStream errors = byte_stream_create(&g_ctx.allocator);
        Lexer lexer = lexer_create("'\\x12'  '\\x3'", 0, &errors);
        Token token = {0};

        token = scan_token(&lexer);
        TKN_TEST_CHAR(token, '\x12');

        token = scan_token(&lexer);
        TKN_TEST_CHAR(token, '\x3');

        token = scan_token(&lexer);
        assert(token.kind == TKN_EOF);
        assert(errors.num_chunks == 0);

        lexer_destroy(&lexer);
    }

    // Test errors when lexing escaped hex chars
    {
        ByteStream errors = byte_stream_create(&g_ctx.allocator);
        Lexer lexer = lexer_create("'' 'a '\n' '\\z' '\\0'", 0, &errors);
        Token token = {0};

        token = scan_token(&lexer);
        assert(errors.num_chunks == 1);

        token = scan_token(&lexer);
        assert(errors.num_chunks == 2);

        token = scan_token(&lexer);
        assert(errors.num_chunks == 3);

        token = scan_token(&lexer);
        assert(errors.num_chunks == 4);

        token = scan_token(&lexer);
        TKN_TEST_CHAR(token, '\0');

        token = scan_token(&lexer);
        assert(token.kind == TKN_EOF);

        print_errors(&errors);
        lexer_destroy(&lexer);
    }

    // Test basic string literals
    {
        ByteStream errors = byte_stream_create(&g_ctx.allocator);
        const char* str = "\"hello world\" \"a\\nb\" \n \"\\x50 a \\x51\" \"\" \"\\\"nested\\\"\"";
        Lexer lexer = lexer_create(str, 0, &errors);
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
        assert(errors.num_chunks == 0);

        lexer_destroy(&lexer);
    }

    // Test errors when scanning string literals
    {
        ByteStream errors = byte_stream_create(&g_ctx.allocator);
        const char* str = "\"\n\" \"\\xTF\" \"\\W\" \"unclosed";
        Lexer lexer = lexer_create(str, 0, &errors);

        scan_token(&lexer);
        assert(errors.num_chunks == 1);

        scan_token(&lexer);
        assert(errors.num_chunks == 2);

        scan_token(&lexer);
        assert(errors.num_chunks == 3);

        scan_token(&lexer);
        assert(errors.num_chunks == 4);

        print_errors(&errors);
        lexer_destroy(&lexer);
    }

    // Test basic identifiers
    {
        ByteStream errors = byte_stream_create(&g_ctx.allocator);
        const char* str = "var x1a x11 _abc abc_ _ab_c_ i";
        Lexer lexer = lexer_create(str, 0, &errors);
        Token token = {0};

        token = scan_token(&lexer);
        TKN_TEST_IDEN(token, "var");

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
        assert(errors.num_chunks == 0);

        lexer_destroy(&lexer);
    }

    // Test invalid identifier combinations.
    {
        ByteStream errors = byte_stream_create(&g_ctx.allocator);
        const char* str = "1var";
        Lexer lexer = lexer_create(str, 0, &errors);
        Token token = {0};

        token = scan_token(&lexer);
        assert(errors.num_chunks == 1);

        token = scan_token(&lexer);
        assert(token.kind == TKN_EOF);

        print_errors(&errors);
        lexer_destroy(&lexer);
    }
}

void test_allocator(void)
{
    Allocator allocator = {0};

    allocator = allocator_create(512);

    void* m = mem_allocate(&allocator, 256, DEFAULT_ALIGN, true);
    assert(m);
    assert((allocator.at - allocator.buffer) >= 256);

    unsigned char* old_buffer = allocator.buffer;
    m = mem_allocate(&allocator, 1024, DEFAULT_ALIGN, false);
    assert(m);
    assert(old_buffer != allocator.buffer);
    assert((allocator.at - allocator.buffer) >= 1024);

    allocator_destroy(&allocator);
    assert(!allocator.buffer);
    assert(!allocator.at);
    assert(!allocator.end);

    allocator = allocator_create(512);

    old_buffer = allocator.buffer;
    m = mem_allocate(&allocator, 1024, DEFAULT_ALIGN, false);
    assert(m);
    assert(old_buffer != allocator.buffer);
    assert((allocator.end - allocator.buffer) >= 1024);

    allocator_reset(&allocator);
    assert(allocator.buffer);
    assert(allocator.at == allocator.buffer);
    assert(allocator.end > allocator.buffer);

    allocator_destroy(&allocator);
    assert(!allocator.buffer);
    assert(!allocator.at);
    assert(!allocator.end);

    // Test allocator state restoration.
    allocator = allocator_create(512);

    m = mem_allocate(&allocator, 16, DEFAULT_ALIGN, false);
    assert(m);
    assert((allocator.at - allocator.buffer) >= 16);
    assert((allocator.at - allocator.buffer) <= 64);

    AllocatorState state = allocator_get_state(&allocator);
    {
        m = mem_allocate(&allocator, 64, DEFAULT_ALIGN, false);
        assert(m);
        assert((allocator.at - allocator.buffer) >= 64 + 16);
    }
    allocator_restore_state(state);

    assert((allocator.at - allocator.buffer) >= 16);
    assert((allocator.at - allocator.buffer) <= 64);

    allocator_destroy(&allocator);
    assert(!allocator.buffer);
    assert(!allocator.at);
    assert(!allocator.end);

    // Test allocator state restoration.
    allocator = allocator_create(512);

    m = mem_allocate(&allocator, 16, DEFAULT_ALIGN, false);
    assert(m);
    assert((allocator.at - allocator.buffer) >= 16);
    assert((allocator.at - allocator.buffer) <= 64);

    old_buffer = allocator.buffer;
    state = allocator_get_state(&allocator);
    {
        m = mem_allocate(&allocator, 2048, DEFAULT_ALIGN, false);
        assert(m);
        assert(old_buffer != allocator.buffer);
        assert((allocator.at - allocator.buffer) >= 2048);
    }
    allocator_restore_state(state);

    assert(old_buffer == allocator.buffer);
    assert((allocator.at - allocator.buffer) >= 16);
    assert((allocator.at - allocator.buffer) <= 64);

    allocator_destroy(&allocator);
    assert(!allocator.buffer);
    assert(!allocator.at);
    assert(!allocator.end);
}

void test_array(void)
{
    Allocator allocator = allocator_create(1024);

    // Test array create and len/cap tracking.
    {
        int* a = array_create(&allocator, int, 128);
        assert(array_len(a) == 0);
        assert(array_cap(a) == 128);

        for (int i = 0; i < 20; i++) {
            array_push(a, i);
            assert(array_back(a) == i);
            assert(array_len(a) == (size_t)i + 1);
            assert(array_cap(a) == 128);
        }
    }

    // Test array reallocation.
    {
        int* a = array_create(&allocator, int, 16);
        int* old_a = a;

        for (int i = 0; i < 256; i++) {
            array_push(a, i);
            assert(array_back(a) == i);
            assert(array_len(a) == (size_t)i + 1);
            assert(array_cap(a) >= array_len(a));
        }
        assert(old_a != a);
        assert(array_cap(a) == array_len(a));

        array_push(a, 257);
        assert(array_cap(a) > array_len(a));
    }

    // Test array clearing
    {
        int* a = array_create(&allocator, int, 16);
        array_push(a, 10);
        assert(array_len(a) > 0);

        array_clear(a);
        assert(array_len(a) == 0);
        assert(array_cap(a) > 0);
    }

    // Test insertion
    {
        int* a = array_create(&allocator, int, 8);
        for (int i = 0; i < 4; i++) {
            array_push(a, i);
        }
        assert(array_len(a) == 4);

        array_insert(a, 1, 100);
        assert(array_len(a) == 5);
        assert(a[0] == 0);
        assert(a[1] == 100);
        assert(a[2] == 1);
        assert(a[3] == 2);
        assert(a[4] == 3);
    }

    // Test array pop
    {
        int* a = array_create(&allocator, int, 8);

        array_push(a, 333);

        size_t old_len = array_len(a);
        int geo = array_pop(a);

        assert(array_len(a) == old_len - 1);
        assert(geo == 333);
    }

    // Test array remove.
    {
        int* a = array_create(&allocator, int, 10);
        for (int i = 0; i < 8; i++) {
            array_push(a, i);
        }
        assert(array_len(a) == 8);

        int rindex = 3;
        array_remove(a, rindex);
        assert(array_len(a) == 7);

        for (int i = 0; i < 7; i++) {
            if (i < rindex) {
                assert(a[i] == i);
            } else {
                assert(a[i] == i + 1);
            }
        }
    }

    // Test array remove (swap last).
    {
        int* a = array_create(&allocator, int, 10);
        for (int i = 0; i < 8; i++) {
            array_push(a, i);
        }
        assert(array_len(a) == 8);

        int rindex = 3;
        array_remove_swap(a, rindex);
        assert(array_len(a) == 7);
        assert(a[rindex] == 7);
    }

    allocator_destroy(&allocator);
}

void test_hash_map(void)
{
    HashMap map = hash_map(21, NULL);

    for (uint64_t i = 1; i <= (1 << 20); ++i) {
        uint64_t* r = hash_map_put(&map, i, i);

        assert(r);
        assert(*r == i);
    }

    for (uint64_t i = 1; i <= (1 << 20); ++i) {
        uint64_t* r = hash_map_get(&map, i);

        assert(r);
        assert(*r == i);
    }

    printf("cap = %lu, len = %lu\n", map.cap, map.len);

    hash_map_destroy(&map);
}

void test_interning(void)
{
    Allocator arena = allocator_create(4096);
    HashMap strmap = hash_map(8, NULL);

    const char* a = "hello";
    const char* b = "hello!";
    const char* a_in = str_intern(&arena, &strmap, a, strlen(a));
    const char* b_in = str_intern(&arena, &strmap, b, strlen(b));

    assert(a != a_in);
    assert(b != b_in);
    assert(a_in == str_intern(&arena, &strmap, a, strlen(a)));
    assert(b_in == str_intern(&arena, &strmap, b, strlen(b)));
    assert(strmap.len == 2);
    assert(a_in != b_in);

    hash_map_destroy(&strmap);
    allocator_destroy(&arena);
}

int main(void)
{
    g_ctx.allocator = allocator_create(4096);

    printf("Nibble tests!\n");
    test_lexer();
    test_allocator();
    test_array();
    test_hash_map();
    test_interning();

    allocator_destroy(&g_ctx.allocator);
}
