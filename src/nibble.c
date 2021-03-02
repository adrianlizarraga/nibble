#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "allocator.c"
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

int main(void)
{
    printf("Nibble!\n");
    test_lexer();
    test_mem_arena();
}
