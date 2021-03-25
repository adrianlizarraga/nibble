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
    Token ptoken;
} Parser;

Parser parser_create(Allocator* allocator, const char* str, ProgPos pos, ByteStream* errors);
void parser_destroy(Parser* parser);

bool next_token(Parser* parser);
bool is_token(Parser* parser, TokenKind kind);
bool match_token_next(Parser* parser, TokenKind kind);
bool match_keyword_next(Parser* parser, Keyword kw);
bool expect_token_next(Parser* parser, TokenKind kind);
#endif
