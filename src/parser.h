#ifndef NIBBLE_PARSER_H
#define NIBBLE_PARSER_H
#include "nibble.h"
#include "allocator.h"
#include "lexer.h"

typedef struct Parser {
    Allocator* allocator;
    ByteStream* errors;
    ProgPos start;
    Lexer lexer;
    Token token;
} Parser;

Parser parser_create(Allocator* allocator, const char* str, ProgPos pos, ByteStream* errors);
void parser_destroy(Parser* parser);

bool next_token(Parser* parser);
bool is_token(Parser* parser, TokenKind kind);
bool match_token(Parser* parser, TokenKind kind);
bool expect_token(Parser* parser, TokenKind kind);
#endif
