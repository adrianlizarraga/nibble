//#define NDEBUG 1
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "allocator.c"
#include "array.c"
#include "hash_map.c"
#include "lexer.c"

typedef struct TestProgContext {
    int num_errors;
    Allocator allocator;
} TestProgContext;

static TestProgContext g_ctx;

static void test_on_error(void* data, ProgPos pos, const char* msg)
{
    if (data) {
        TestProgContext* c = data;

        c->num_errors += 1;
        printf("[ERROR]:%u: %s\n", pos, msg);
    }
}

static const char* test_on_str(void* data, ProgPos pos, const char* str, size_t len)
{
    (void)pos;

    if (data) {
        TestProgContext* c = data;

        char* dup = new_array(&c->allocator, char, len + 1, false);

        for (size_t i = 0; i < len; ++i) {
            dup[i] = str[i];
        }
        dup[len] = '\0';

        return dup;
    }

    return NULL;
}

static const char* test_on_identifier(void* data, ProgPos pos, const char* str, size_t len)
{
    (void)pos;

    if (data) {
        TestProgContext* c = data;

        char* dup = new_array(&c->allocator, char, len + 1, false);

        for (size_t i = 0; i < len; ++i) {
            dup[i] = str[i];
        }
        dup[len] = '\0';

        return dup;
    }

    return NULL;
}

#define TKN_TEST_POS(tk, tp, a, b)                                                                                     \
    do {                                                                                                               \
        assert((tk.kind == tp));                                                                                       \
        assert((tk.start == a));                                                                                       \
        assert((tk.end == b));                                                                                         \
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
        assert((tk.kind == TKN_IDENTIFIER));                                                                           \
        assert(strcmp(tk.tidentifier.value, v) == 0);                                                                  \
    } while (0)
static void test_init_lexer(Lexer* lexer, const char* str, ProgPos start)
{
    free_lexer(lexer);
    *lexer = lexer_from_str(str, start);
    lexer->client.data = &g_ctx;
    lexer->client.on_error = test_on_error;
    lexer->client.on_str = test_on_str;
    lexer->client.on_identifier = test_on_identifier;

    g_ctx.num_errors = 0;
}

static void test_lexer(void)
{
    Lexer lexer = {0};

    // Test basic tokens, newlines, and c++ comments.
    unsigned int i = 10;
    test_init_lexer(&lexer, "(+[]-*%&|^<>) && || >> << == >= <= = += -= *= /= &= |= ^= %= \n  //++--\n{;:,./}", i);

    next_token(&lexer);
    TKN_TEST_POS(lexer.token, TKN_LPAREN, i, ++i);

    next_token(&lexer);
    TKN_TEST_POS(lexer.token, TKN_PLUS, i, ++i);

    next_token(&lexer);
    TKN_TEST_POS(lexer.token, TKN_LBRACE, i, ++i);

    next_token(&lexer);
    TKN_TEST_POS(lexer.token, TKN_RBRACE, i, ++i);

    next_token(&lexer);
    TKN_TEST_POS(lexer.token, TKN_MINUS, i, ++i);

    next_token(&lexer);
    TKN_TEST_POS(lexer.token, TKN_ASTERISK, i, ++i);

    next_token(&lexer);
    TKN_TEST_POS(lexer.token, TKN_MOD, i, ++i);

    next_token(&lexer);
    TKN_TEST_POS(lexer.token, TKN_AND, i, ++i);

    next_token(&lexer);
    TKN_TEST_POS(lexer.token, TKN_OR, i, ++i);

    next_token(&lexer);
    TKN_TEST_POS(lexer.token, TKN_XOR, i, ++i);

    next_token(&lexer);
    TKN_TEST_POS(lexer.token, TKN_LT, i, ++i);

    next_token(&lexer);
    TKN_TEST_POS(lexer.token, TKN_GT, i, ++i);

    next_token(&lexer);
    TKN_TEST_POS(lexer.token, TKN_RPAREN, i, ++i);

    i++;
    next_token(&lexer);
    TKN_TEST_POS(lexer.token, TKN_LOGIC_AND, i, i + 2);
    i += 3;

    next_token(&lexer);
    TKN_TEST_POS(lexer.token, TKN_LOGIC_OR, i, i + 2);
    i += 3;

    next_token(&lexer);
    TKN_TEST_POS(lexer.token, TKN_RSHIFT, i, i + 2);
    i += 3;

    next_token(&lexer);
    TKN_TEST_POS(lexer.token, TKN_LSHIFT, i, i + 2);
    i += 3;

    next_token(&lexer);
    TKN_TEST_POS(lexer.token, TKN_EQ, i, i + 2);
    i += 3;

    next_token(&lexer);
    TKN_TEST_POS(lexer.token, TKN_GTEQ, i, i + 2);
    i += 3;

    next_token(&lexer);
    TKN_TEST_POS(lexer.token, TKN_LTEQ, i, i + 2);
    i += 3;

    next_token(&lexer);
    TKN_TEST_POS(lexer.token, TKN_ASSIGN, i, i + 1);
    i += 2;

    next_token(&lexer);
    TKN_TEST_POS(lexer.token, TKN_ADD_ASSIGN, i, i + 2);
    i += 3;

    next_token(&lexer);
    TKN_TEST_POS(lexer.token, TKN_SUB_ASSIGN, i, i + 2);
    i += 3;

    next_token(&lexer);
    TKN_TEST_POS(lexer.token, TKN_MUL_ASSIGN, i, i + 2);
    i += 3;

    next_token(&lexer);
    TKN_TEST_POS(lexer.token, TKN_DIV_ASSIGN, i, i + 2);
    i += 3;

    next_token(&lexer);
    TKN_TEST_POS(lexer.token, TKN_AND_ASSIGN, i, i + 2);
    i += 3;

    next_token(&lexer);
    TKN_TEST_POS(lexer.token, TKN_OR_ASSIGN, i, i + 2);
    i += 3;

    next_token(&lexer);
    TKN_TEST_POS(lexer.token, TKN_XOR_ASSIGN, i, i + 2);
    i += 3;

    next_token(&lexer);
    TKN_TEST_POS(lexer.token, TKN_MOD_ASSIGN, i, i + 2);
    i += 3;

    i += 10;
    next_token(&lexer);
    TKN_TEST_POS(lexer.token, TKN_LBRACKET, i, ++i);

    next_token(&lexer);
    TKN_TEST_POS(lexer.token, TKN_SEMICOLON, i, ++i);

    next_token(&lexer);
    TKN_TEST_POS(lexer.token, TKN_COLON, i, ++i);

    next_token(&lexer);
    TKN_TEST_POS(lexer.token, TKN_COMMA, i, ++i);

    next_token(&lexer);
    TKN_TEST_POS(lexer.token, TKN_DOT, i, ++i);

    next_token(&lexer);
    TKN_TEST_POS(lexer.token, TKN_DIV, i, ++i);

    next_token(&lexer);
    TKN_TEST_POS(lexer.token, TKN_RBRACKET, i, ++i);

    next_token(&lexer);
    TKN_TEST_POS(lexer.token, TKN_EOF, i, ++i);

    // Test nested c-style comments
    test_init_lexer(&lexer, "/**** 1 /* 2 */ \n***/+-", 0);

    next_token(&lexer);
    TKN_TEST_POS(lexer.token, TKN_PLUS, 21, 22);

    next_token(&lexer);
    TKN_TEST_POS(lexer.token, TKN_MINUS, 22, 23);

    next_token(&lexer);
    TKN_TEST_POS(lexer.token, TKN_EOF, 23, 24);

    // Test error when have unclosed c-style comments
    test_init_lexer(&lexer, "/* An unclosed comment", 0);

    next_token(&lexer);
    TKN_TEST_POS(lexer.token, TKN_EOF, 22, 23);
    assert(g_ctx.num_errors == 1);

    test_init_lexer(&lexer, "/* An unclosed comment\n", 0);

    next_token(&lexer);
    TKN_TEST_POS(lexer.token, TKN_EOF, strlen(lexer.str), strlen(lexer.str) + 1);
    assert(g_ctx.num_errors == 1);

    // Test integer literals
    test_init_lexer(&lexer, "123 333\n0xFF 0b0111 011 0", 0);

    next_token(&lexer);
    TKN_TEST_INT(lexer.token, TKN_INT_DEC, 123);

    next_token(&lexer);
    TKN_TEST_INT(lexer.token, TKN_INT_DEC, 333);

    next_token(&lexer);
    TKN_TEST_INT(lexer.token, TKN_INT_HEX, 0xFF);

    next_token(&lexer);
    TKN_TEST_INT(lexer.token, TKN_INT_BIN, 7);

    next_token(&lexer);
    TKN_TEST_INT(lexer.token, TKN_INT_OCT, 9);

    next_token(&lexer);
    TKN_TEST_INT(lexer.token, TKN_INT_DEC, 0);

    next_token(&lexer);
    assert(lexer.token.kind == TKN_EOF);

    test_init_lexer(&lexer, "0Z 0b3 09 1A\n999999999999999999999999", 0);

    next_token(&lexer);
    assert(g_ctx.num_errors == 1);

    next_token(&lexer);
    assert(g_ctx.num_errors == 2);

    next_token(&lexer);
    assert(g_ctx.num_errors == 3);

    next_token(&lexer);
    assert(g_ctx.num_errors == 4);

    next_token(&lexer);
    assert(g_ctx.num_errors == 5);

    next_token(&lexer);
    assert(lexer.token.kind == TKN_EOF);

    // Test floating point literals
    test_init_lexer(&lexer, "1.23 .23 1.33E2", 0);

    next_token(&lexer);
    TKN_TEST_FLOAT(lexer.token, 1.23);

    next_token(&lexer);
    TKN_TEST_FLOAT(lexer.token, .23);

    next_token(&lexer);
    TKN_TEST_FLOAT(lexer.token, 1.33E2);

    test_init_lexer(&lexer, "1.33ea 1.33e100000000000", 0);

    next_token(&lexer);
    assert(g_ctx.num_errors == 1);

    next_token(&lexer);
    assert(g_ctx.num_errors == 2);

    // Test character literals
    test_init_lexer(&lexer,
                    "'a' '1' ' ' '\\0' '\\a' '\\b' '\\f' '\\n' '\\r' '\\t' '\\v' "
                    "'\\\\' '\\'' '\\\"' '\\?'",
                    0);

    next_token(&lexer);
    TKN_TEST_CHAR(lexer.token, 'a');

    next_token(&lexer);
    TKN_TEST_CHAR(lexer.token, '1');

    next_token(&lexer);
    TKN_TEST_CHAR(lexer.token, ' ');

    next_token(&lexer);
    TKN_TEST_CHAR(lexer.token, '\0');

    next_token(&lexer);
    TKN_TEST_CHAR(lexer.token, '\a');

    next_token(&lexer);
    TKN_TEST_CHAR(lexer.token, '\b');

    next_token(&lexer);
    TKN_TEST_CHAR(lexer.token, '\f');

    next_token(&lexer);
    TKN_TEST_CHAR(lexer.token, '\n');

    next_token(&lexer);
    TKN_TEST_CHAR(lexer.token, '\r');

    next_token(&lexer);
    TKN_TEST_CHAR(lexer.token, '\t');

    next_token(&lexer);
    TKN_TEST_CHAR(lexer.token, '\v');

    next_token(&lexer);
    TKN_TEST_CHAR(lexer.token, '\\');

    next_token(&lexer);
    TKN_TEST_CHAR(lexer.token, '\'');

    next_token(&lexer);
    TKN_TEST_CHAR(lexer.token, '"');

    next_token(&lexer);
    TKN_TEST_CHAR(lexer.token, '?');

    next_token(&lexer);
    assert(lexer.token.kind == TKN_EOF);
    assert(g_ctx.num_errors == 0);

    test_init_lexer(&lexer, "'\\x12'  '\\x3'", 0);

    next_token(&lexer);
    TKN_TEST_CHAR(lexer.token, '\x12');

    next_token(&lexer);
    TKN_TEST_CHAR(lexer.token, '\x3');

    test_init_lexer(&lexer, "'' 'a '\n' '\\z' '\\0'", 0);

    next_token(&lexer);
    assert(g_ctx.num_errors == 1);

    next_token(&lexer);
    assert(g_ctx.num_errors == 2);

    next_token(&lexer);
    assert(g_ctx.num_errors == 3);

    next_token(&lexer);
    assert(g_ctx.num_errors == 4);

    next_token(&lexer);
    TKN_TEST_CHAR(lexer.token, '\0');

    next_token(&lexer);
    assert(lexer.token.kind == TKN_EOF);

    // Test basic string literals
    {
        const char* str = "\"hello world\" \"a\\nb\" \n \"\\x50 a \\x51\" \"\" \"\\\"nested\\\"\"";
        test_init_lexer(&lexer, str, 0);

        next_token(&lexer);
        TKN_TEST_STR(lexer.token, "hello world");

        next_token(&lexer);
        TKN_TEST_STR(lexer.token, "a\nb");

        next_token(&lexer);
        TKN_TEST_STR(lexer.token, "\x50 a \x51");

        next_token(&lexer);
        TKN_TEST_STR(lexer.token, "");

        next_token(&lexer);
        TKN_TEST_STR(lexer.token, "\"nested\"");
    }

    // Test errors when scanning string literals
    {
        const char* str = "\"\n\" \"\\xTF\" \"\\W\" \"unclosed";
        test_init_lexer(&lexer, str, 0);

        next_token(&lexer);
        assert(g_ctx.num_errors == 1);

        next_token(&lexer);
        assert(g_ctx.num_errors == 2);

        next_token(&lexer);
        assert(g_ctx.num_errors == 3);

        next_token(&lexer);
        assert(g_ctx.num_errors == 4);
    }

    // Test basic identifiers
    {
        const char* str = "var x1a x11 _abc abc_ _ab_c_ i";
        test_init_lexer(&lexer, str, 0);

        next_token(&lexer);
        TKN_TEST_IDEN(lexer.token, "var");

        next_token(&lexer);
        TKN_TEST_IDEN(lexer.token, "x1a");

        next_token(&lexer);
        TKN_TEST_IDEN(lexer.token, "x11");

        next_token(&lexer);
        TKN_TEST_IDEN(lexer.token, "_abc");

        next_token(&lexer);
        TKN_TEST_IDEN(lexer.token, "abc_");

        next_token(&lexer);
        TKN_TEST_IDEN(lexer.token, "_ab_c_");

        next_token(&lexer);
        TKN_TEST_IDEN(lexer.token, "i");
    }

    // Test invalid identifier combinations.
    {
        const char* str = "1var";
        test_init_lexer(&lexer, str, 0);

        next_token(&lexer);
        assert(g_ctx.num_errors == 1);

        next_token(&lexer);
        assert(lexer.token.kind == TKN_EOF);
    }
    free_lexer(&lexer);
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
    HashMap map = hash_map(24, NULL);

    for (uint64_t i = 1; i <= (1 << 23); ++i) {
        uint64_t* r = hash_map_put(&map, i, i);

        assert(r);
        assert(*r == i);
    }

    for (uint64_t i = 1; i <= (1 << 23); ++i) {
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
