//
//
//

#ifndef GRIM_LEX_H
#define GRIM_LEX_H

#include "common.h"
#include "tok.h"

typedef struct Lexer {
    String      source;
    const char* at;
} Lexer;

Lexer   lexer_init      (String source);
// @TODO: Add a way to store comments. We don't want them with the stream of
// tokens, but we do want to keep track of them for tools and the c code generator.
bool    lexer_next_token(Lexer *lexer, Token *out_token);

DECL_TEST(lex);

#endif // GRIM_LEX_H

