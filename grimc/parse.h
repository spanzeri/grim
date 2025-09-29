#ifndef GRIMC_PARSE_H
#define GRIMC_PARSE_H

#include "common.h"
#include "lex.h"
#include "ast.h"

typedef struct Parse_Context {
    Lexer_Context   lexer;
    Token           current;
    Token           previous;
    Arena           ast_arena;
} Parse_Context;

Parse_Context   parse_init(const char* source);
void            parse_shutdown(Parse_Context* p);

void            parse_begin(Parse_Context* p);
void            parse_end(Parse_Context* p);

Stmt*           parse_stmt(Parse_Context* p);
Decl*           parse_decl(Parse_Context* p);
Expr*           parse_expr(Parse_Context* p);

DECL_TEST(parse);

#endif // GRIMC_PARSE_H
