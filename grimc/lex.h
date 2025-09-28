//
//
//

#ifndef GRIM_LEX_H
#define GRIM_LEX_H

#include "tok.h"

typedef struct Lexer_Context {
    const char *source;
    const char *at;
} Lexer_Context;

Lexer_Context   lexer_init      (const char *source);
Token           lexer_next_token(Lexer_Context *l);

DECL_TEST(lex);

#endif // GRIM_LEX_H

