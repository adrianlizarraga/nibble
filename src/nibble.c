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
} TestProgContext;

static TestProgContext g_ctx;

static void test_on_error(void* data, ProgPos pos, const char* msg)
{
    (void)pos;
    (void)msg;

    if (data) {
        TestProgContext* c = data;

        c->num_errors += 1;
    }
}

#define TKN_TEST_POS(tk, tp, a, b)                                                                                     \
    do {                                                                                                               \
        assert((tk.type == tp));                                                                                       \
        assert((tk.start == a));                                                                                       \
        assert((tk.end == b));                                                                                         \
    } while (0)
#define TKN_TEST_INT(tk, b, v)                                                                                         \
    do {                                                                                                               \
        assert((tk.type == TKN_INT));                                                                                  \
        assert((tk.tint.base = b));                                                                                    \
        assert((tk.tint.value == v));                                                                                  \
    } while (0)
#define TKN_TEST_FLOAT(tk, v)                                                                                          \
    do {                                                                                                               \
        assert((tk.type == TKN_FLOAT));                                                                                \
        assert((tk.tfloat.value == v));                                                                                \
    } while (0)
#define TKN_TEST_CHAR(tk, v)                                                                                           \
    do {                                                                                                               \
        assert((tk.type == TKN_CHAR));                                                                                 \
        assert((tk.tchar.value == v));                                                                                 \
    } while (0)

static void test_init_lexer(Lexer* lexer, const char* str, ProgPos start)
{

    init_lexer(lexer, str, start);
    memset(&g_ctx, 0, sizeof(TestProgContext));
    lexer->client.data = &g_ctx;
    lexer->client.on_error = test_on_error;
}

static void test_lexer(void)
{
    Lexer lexer = {0};

    // Test basic 1 character tokens, newlines, and c++ comments.
    unsigned int i = 10;
    test_init_lexer(&lexer, "(+[]-)  \n  //++--\n{;:,./}", i);

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
    TKN_TEST_POS(lexer.token, TKN_RPAREN, i, ++i);

    i += 12;
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
    test_init_lexer(&lexer, "123 333\n0xFF 0b0111 011", 0);

    next_token(&lexer);
    TKN_TEST_INT(lexer.token, 10, 123);

    next_token(&lexer);
    TKN_TEST_INT(lexer.token, 10, 333);

    next_token(&lexer);
    TKN_TEST_INT(lexer.token, 16, 0xFF);

    next_token(&lexer);
    TKN_TEST_INT(lexer.token, 2, 7);

    next_token(&lexer);
    TKN_TEST_INT(lexer.token, 8, 9);

    next_token(&lexer);
    assert(lexer.token.type == TKN_EOF);

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
    assert(lexer.token.type == TKN_EOF);

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
    assert(lexer.token.type == TKN_EOF);
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
    assert(lexer.token.type == TKN_EOF);
}

void test_mem_arena(void)
{
    MemArena arena = {0};

    arena = mem_arena(NULL, 512);

    void* m = mem_allocate(&arena, 256, DEFAULT_ALIGN, true);
    assert(m);
    assert((arena.at - arena.buffer) >= 256);

    unsigned char* old_buffer = arena.buffer;
    m = mem_allocate(&arena, 1024, DEFAULT_ALIGN, false);
    assert(m);
    assert(old_buffer != arena.buffer);
    assert((arena.at - arena.buffer) >= 1024);

    mem_arena_destroy(&arena);
    assert(!arena.buffer);
    assert(!arena.at);
    assert(!arena.end);

    arena = mem_arena(NULL, 512);

    old_buffer = arena.buffer;
    m = mem_allocate(&arena, 1024, DEFAULT_ALIGN, false);
    assert(m);
    assert(old_buffer != arena.buffer);
    assert((arena.end - arena.buffer) >= 1024);

    mem_arena_reset(&arena);
    assert(arena.buffer);
    assert(arena.at == arena.buffer);
    assert(arena.end > arena.buffer);

    mem_arena_destroy(&arena);
    assert(!arena.buffer);
    assert(!arena.at);
    assert(!arena.end);

    // Test arena state restoration.
    arena = mem_arena(NULL, 512);

    m = mem_allocate(&arena, 16, DEFAULT_ALIGN, false);
    assert(m);
    assert((arena.at - arena.buffer) >= 16);
    assert((arena.at - arena.buffer) <= 64);

    MemArenaState state = mem_arena_snapshot(&arena);
    {
        m = mem_allocate(&arena, 64, DEFAULT_ALIGN, false);
        assert(m);
        assert((arena.at - arena.buffer) >= 64 + 16);
    }
    mem_arena_restore(state);

    assert((arena.at - arena.buffer) >= 16);
    assert((arena.at - arena.buffer) <= 64);

    mem_arena_destroy(&arena);
    assert(!arena.buffer);
    assert(!arena.at);
    assert(!arena.end);

    // Test arena state restoration.
    arena = mem_arena(NULL, 512);

    m = mem_allocate(&arena, 16, DEFAULT_ALIGN, false);
    assert(m);
    assert((arena.at - arena.buffer) >= 16);
    assert((arena.at - arena.buffer) <= 64);

    old_buffer = arena.buffer;
    state = mem_arena_snapshot(&arena);
    {
        m = mem_allocate(&arena, 2048, DEFAULT_ALIGN, false);
        assert(m);
        assert(old_buffer != arena.buffer);
        assert((arena.at - arena.buffer) >= 2048);
    }
    mem_arena_restore(state);

    assert(old_buffer == arena.buffer);
    assert((arena.at - arena.buffer) >= 16);
    assert((arena.at - arena.buffer) <= 64);

    mem_arena_destroy(&arena);
    assert(!arena.buffer);
    assert(!arena.at);
    assert(!arena.end);
}

void test_array(void)
{
    MemArena arena = mem_arena(NULL, 1024);

    // Test array create and len/cap tracking.
    {
        int* a = array_create(&arena, 128);
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
        int* a = array_create(&arena, 16);
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
        int* a = array_create(&arena, 16);
        array_push(a, 10);
        assert(array_len(a) > 0);

        array_clear(a);
        assert(array_len(a) == 0);
        assert(array_cap(a) > 0);
    }

    // Test insertion
    {
        int* a = array_create(&arena, 8);
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
        int* a = array_create(&arena, 8);

        array_push(a, 333);

        size_t old_len = array_len(a);
        int geo = array_pop(a);

        assert(array_len(a) == old_len - 1);
        assert(geo == 333);
    }

    // Test array remove.
    {
        int* a = array_create(&arena, 10);
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
        int* a = array_create(&arena, 10);
        for (int i = 0; i < 8; i++) {
            array_push(a, i);
        }
        assert(array_len(a) == 8);

        int rindex = 3;
        array_remove_swap(a, rindex);
        assert(array_len(a) == 7);
        assert(a[rindex] == 7);
    }

    mem_arena_destroy(&arena);
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

int main(void)
{
    printf("Nibble!\n");
    test_lexer();
    test_mem_arena();
    test_array();
    test_hash_map();
}
