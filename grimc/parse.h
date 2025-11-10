#ifndef GRIMC_PARSE_H
#define GRIMC_PARSE_H

#include "common.h"
#include "lex.h"
#include "ast.h"

typedef struct Parser {
    Token*      tokens;
    int         tok_index;
    Token       current;
    Token       previous;
    Arena       ast_arena;
    Arena       string_arena;
    const char* filepath;
} Parser;

Parser  parser_init(String source, const char* filepath);
void    parser_shutdown(Parser* p);

bool    parser_is_at_end(Parser* p);
Stmt*   parser_next_stmt(Parser* p);

DECL_TEST(parse);

#endif // GRIMC_PARSE_H

