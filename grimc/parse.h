#ifndef GRIMC_PARSE_H
#define GRIMC_PARSE_H

#include "common.h"
#include "lex.h"
#include "ast.h"

typedef struct Parse_Context {
    Lexer_Context   lexer;
    Token           current;
    Token           previous;
} Parse_Context;

Parse_Context   parse_init(const char* source);

Stmt* parse_stmt(Parse_Context* pctx);
Decl* parse_decl(Parse_Context* pctx);
Expr* parse_expr(Parse_Context* pctx);

DECL_TEST(parse);

#endif // GRIMC_PARSE_H
