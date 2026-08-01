#include "parse.h"

static Stmt*    parse_stmt(Parser* p);
static void     advance(Parser* p);
static Token    peek(Parser* p, int offset);
static bool     match(Parser* p, Token_Kind kind);
static bool     match_keyword(Parser* p, Keyword kw);
static bool     consume(Parser* p, Token_Kind kind, const char* err_msg);
static bool     consume_keyword(Parser* p, Keyword kw, const char* err_msg);

static bool     is_keyword(Token tok, Keyword kw);
static String   dup_string(Arena* arena, String str);
static void     end_stmt(Parser* p);

static Stmt*    parse_expr_stmt(Parser* p);
static Stmt*    parse_return_stmt(Parser* p);
static Stmt*    parse_if_stmt(Parser* p);
static Stmt*    parse_for_stmt(Parser* p);
static Stmt*    parse_switch_stmt(Parser* p);
static Stmt*    parse_break_stmt(Parser* p);
static Stmt*    parse_continue_stmt(Parser* p);

static Expr*    parse_expr(Parser* p);
static Expr*    parse_single_expr(Parser* p);

Parser parser_init(String source, const char* filepath)
{
    Parser parser = (Parser){0};
    parser.filepath = filepath;

    // Lex all the tokens up front. Make an educated guess at the number of
    // tokens to avoid too many re-allocations.
    // @OPTIMIZE: A virtual memory based arena could be used to ensure we never
    // re-allocate the token array.
    // @TODO: Verify the heuristic. Is 4 below a good value? Should it be 3 or 5?
    // Need to take some real-world source files and measure the ratio between
    // source size and token count.
    usize source_size = source.len;
    int estimated_token_count = (int)(source_size / 4); // Average token size
    darray_reserve(parser.tokens, estimated_token_count);

    Lexer lexer = lexer_init(source);
    Token tok;
    while (lexer_next_token(&lexer, &tok)) {
        darray_add(parser.tokens, tok);
        if (tok.kind == TOK_EOF) {
            break;
        }
    }
    parser.current = darray_len(parser.tokens) > 0 ? parser.tokens[0] : (Token){0};

    fprintf(stderr, "Lexed %d tokens from %s\n", darray_len(parser.tokens), filepath);

    return parser;
}

void parser_shutdown(Parser* p)
{
    arena_reset(&p->string_arena);
    arena_reset(&p->ast_arena);
}

bool parser_is_at_end(Parser* p)
{
    return p->tokens[p->tok_index].kind == TOK_EOF;
}

Stmt* parser_next_stmt(Parser* p)
{
    Stmt* stmt = parse_stmt(p);
    ASSERT(stmt != NULL || parser_is_at_end(p), "Expected statement or end of file");
    return stmt;
}

static String* get_stmt_label_ptr(Stmt* stmt)
{
    switch (stmt->kind) {
        case STMT_BLOCK: {
            Block_Stmt* block_stmt = (Block_Stmt*)stmt;
            return &block_stmt->label;
        } break;

        case STMT_FOR: {
            For_Stmt* for_stmt = (For_Stmt*)stmt;
            return &for_stmt->label;
        } break;

        default:
            return NULL;
    }
}

static void set_stmt_label(Stmt* stmt, String label)
{
    String* label_ptr = get_stmt_label_ptr(stmt);
    if (label_ptr) {
        *label_ptr = label;
    }
}

static String get_stmt_label(Stmt* stmt)
{
    String* label_ptr = get_stmt_label_ptr(stmt);
    if (label_ptr) {
        return *label_ptr;
    }
    return (String){0};
}

static Stmt* parse_stmt(Parser* p)
{
    Token current = {0};
    Stmt* stmt = NULL;

restart:
    // Label are unfortunately a bit tricky to handle. We do it here, so we know
    // by the time we reach the switch statement below, we are done.
    if (current.kind == TOK_IDENTIFIER && peek(p, 1).kind == ':' &&
        (peek(p, 2).kind == '{' || is_keyword(peek(p, 2), KW_FOR))) {
        String label = dup_string(&p->string_arena, current.svalue);
        advance(p);
        advance(p);
        stmt = parse_stmt(p);
        if (!stmt) { return NULL; }

        if (!str_is_empty(get_stmt_label(stmt))) {
            syntax_error("Multiple labels on the same block are not allowed.");
            return NULL;
        }

        set_stmt_label(stmt, label);
        return stmt;
    }

    switch (current.kind) {
        case ';':
            advance(p);
            goto restart;
            break;

        case TOK_EOF:
            break;

        case TOK_KEYWORD: {
            switch (current.keyword) {
                case KW_RETURN:     return parse_return_stmt(p);
                case KW_IF:         return parse_if_stmt(p);
                case KW_FOR:        return parse_for_stmt(p);
                case KW_SWITCH:     return parse_switch_stmt(p);
                case KW_BREAK:      return parse_break_stmt(p);
                case KW_CONTINUE:   return parse_continue_stmt(p);
                default:            break;
            }
        } break;

        case '{': {
            advance(p); // Consume '{'
            Stmt** stmts = NULL;
            while (!parser_is_at_end(p) && p->current.kind != '}') {
                Stmt* substmt = parse_stmt(p);
                if (!substmt) {
                    syntax_error("Expected statement in block");
                    darray_free(stmts);
                    return NULL;
                }
                darray_add(stmts, substmt);
            }

            if (!consume(p, '}', "Expected '}' at end of block")) {
                darray_free(stmts);
                return NULL;
            }

            stmt = stmt_block(&p->ast_arena, stmts, darray_len(stmts), (String){0});
            darray_free(stmts);
        } break;

        default: {
            stmt = parse_expr_stmt(p);
        } break;

    }

    return stmt;
}

static void advance(Parser* p)
{
    if (!parser_is_at_end(p)) {
        p->tok_index += 1;
        p->previous = p->current;
        p->current = p->tokens[p->tok_index];
    }
}

static Token peek(Parser* p, int offset)
{
    int index = p->tok_index + offset;
    if (index >= darray_len(p->tokens)) {
        return (Token){};
    }
    return p->tokens[index];
}

static bool match(Parser* p, Token_Kind kind)
{
    if (p->current.kind == kind) {
        advance(p);
        return true;
    }
    return false;
}

static bool match_keyword(Parser* p, Keyword kw)
{
    if (p->current.kind == TOK_KEYWORD && p->current.keyword == kw) {
        advance(p);
        return true;
    }
    return false;
}

static bool consume(Parser* p, Token_Kind kind, const char* err_msg)
{
    if (p->current.kind == kind) {
        advance(p);
        return true;
    }
    syntax_error("%s", err_msg);
    return false;
}

static bool consume_keyword(Parser* p, Keyword kw, const char* err_msg)
{
    if (p->current.kind == TOK_KEYWORD && p->current.keyword == kw) {
        advance(p);
        return true;
    }
    syntax_error("%s", err_msg);
    return false;
}

static bool is_keyword(Token tok, Keyword kw)
{
    return tok.kind == TOK_KEYWORD && tok.keyword == kw;
}

static String dup_string(Arena* arena, String str)
{
    ASSERT(arena);
    char* buffer = arena_alloc(arena, str.len + 1);
    memcpy(buffer, str.data, str.len);
    buffer[str.len] = '\0';
    return (String){ .data = buffer, .len = str.len };
}

static void end_stmt(Parser* p)
{
    if (match(p, ';')) { return; }

    // @TODO: Get the next token. Get the line number of the next token.
    // If we are on a new line, return. If we aren't, this is not allowed.
}

static Stmt* parse_expr_stmt(Parser* p)
{
    Stmt* stmt = NULL;

    Expr* expr = parse_expr(p);
    if (!expr) { return NULL; }

    if (match(p, ':')) {
        Expr* typespec = parse_expr(p);
        if (!typespec) {
            syntax_error("Expected type specifier, '=' or ':' after a declaration.");
            return NULL;
        }

        if (match(p, '=')) {
            Expr* rhs = parse_expr(p);
            if (rhs == NULL) {
                syntax_error("Expected expression after '=' in a variable declaration.");
                return NULL;
            }
            stmt = stmt_decl_var(&p->ast_arena, expr, typespec, rhs);
        }
        else if (match(p, ':')) {
            Expr* rhs = parse_expr(p);
            if (rhs == NULL) {
                syntax_error("Expected expression after ':' in a constant declaration.");
                return NULL;
            }
            stmt = stmt_decl_const(&p->ast_arena, expr, typespec, rhs);
        }
        else {
            // Only: `name: type`. It must be a variable
            stmt = stmt_decl_var(&p->ast_arena, expr, typespec, NULL);
        }
    }
    else if (match(p, TOK_VAR_ASSIGN)) {
        Expr* rhs = parse_expr(p);
        if (rhs == NULL) {
            syntax_error("Expected expression after ':=' in a variable assignment.");
            return NULL;
        }
        stmt = stmt_assignment(&p->ast_arena, TOK_VAR_ASSIGN, expr, rhs);
    }
    else if (match(p, TOK_CONST_ASSIGN)) {
        Expr* rhs = parse_expr(p);
        if (rhs == NULL) {
            syntax_error("Expected expression after '::' in a constant assignment.");
            return NULL;
        }
        stmt = stmt_assignment(&p->ast_arena, TOK_CONST_ASSIGN, expr, rhs);
    }
    else {
        stmt = stmt_expr(&p->ast_arena, expr);
    }

    ASSERT(stmt != NULL);

    // Possibly consume the trailing semicolon or end of file.
    end_stmt(p);
    return stmt;
}

static Stmt* parse_return_stmt(Parser* p)
{
    if (!match_keyword(p, KW_RETURN)) { return NULL; }
    Expr* expr = parse_expr(p);
    Stmt* stmt = stmt_return(&p->ast_arena, expr);
    end_stmt(p);
    return stmt;
}

static Stmt* parse_if_stmt(Parser* p)
{
    if (!match_keyword(p, KW_IF)) { return NULL; }
    Expr* init = NULL;
    Expr* cond = parse_expr(p);
    if (!cond) {
        syntax_error("Expected condition or `init; condition` after `if`");
        return NULL;
    }
    if (match(p, ';')) {
        init = cond;
        cond = parse_expr(p);
        if (!cond) {
            syntax_error("Expected condition after `if` init statement");
            return NULL;
        }
    }
    Stmt* then_branch = parse_stmt(p);
    Stmt* else_branch = NULL;
    if (match_keyword(p, KW_ELSE)) {
        else_branch = parse_stmt(p);
        if (!else_branch) {
            syntax_error("Expected statement after `else`");
            return NULL;
        }
    }
    Stmt* stmt = stmt_if_init(&p->ast_arena, init, cond, then_branch, else_branch);
    return stmt;
}

static Stmt* parse_for_stmt(Parser* p)
{
    // @TODO: Differentiate between traditional for-loops and range-based for-loops.
    if (!match_keyword(p, KW_FOR)) { return NULL; }
    Stmt* init = parse_expr_stmt(p);
    if (init && !match(p, ';')) {
        syntax_error("Expected ';' after `for` init statement");
        return NULL;
    }
    else {
        match(p, ';'); // @IMPROVE: Should we not warn against emtpy `for ;;` statements?
    }

    Expr* cond = parse_expr(p);
    Stmt* post = NULL;
    if (match(p, ';')) {
        post = parse_expr_stmt(p);
    }

    Stmt* body = parse_stmt(p);
    if (!body) {
        syntax_error("Expected statement after `for`");
        return NULL;
    }

    return stmt_for(&p->ast_arena, init, cond, post, body, (String){0});
}

static Stmt* parse_switch_stmt(Parser* p) {
    if (!match_keyword(p, KW_SWITCH)) { return NULL; }
    Expr* expr = parse_expr(p);
    if (!expr) {
        syntax_error("Expected expression after `switch`");
        return NULL;
    }
    consume(p, '{', "Expected '{' after `switch` expression");

    Switch_Case* cases = NULL;
    Switch_Case default_case = {0};

    for (;;) {
        if (match(p, '}')) { break; }
        consume_keyword(p, KW_CASE, "Expected `case` or `}` in `switch` statement");

        if (match(p, '}')) { break; }

        if (match(p, ':')) {
            // This is a `default` case.
            if (default_case.body != NULL) {
                syntax_error("Only a single `default` case is allowed in a `switch` statement");
                return NULL;
            }
            Stmt* case_body = parse_stmt(p);
            if (!case_body) {
                syntax_error("Expected statement after `case`");
                return NULL;
            }
        }
        else {
            Expr* cond = parse_expr(p);
            if (!cond) {
                syntax_error("Expected expression after `case`");
                return NULL;
            }
            Stmt* case_body = parse_stmt(p);
            if (!case_body) {
                syntax_error("Expected statement after `case`");
                return NULL;
            }
            darray_add(cases, (Switch_Case){
                .condition = cond,
                .body      = case_body,
            });
        }
    }

    Stmt* stmt = stmt_switch(&p->ast_arena, cases, darray_len(cases), default_case.body);
    darray_free(cases);
    return stmt;
}

static Stmt* parse_control_stmt(Parser* p, Keyword kw, Stmt* (*create_stmt)(Arena*, String))
{
    if (!match_keyword(p, KW_BREAK)) { return NULL; }
    String label = (String){0};
    if (p->current.kind == TOK_IDENTIFIER) {
        advance(p);
        label = dup_string(&p->string_arena, p->current.svalue);
    }
    Stmt* stmt = create_stmt(&p->ast_arena, label);
    end_stmt(p);
    return stmt;
}

static Stmt* parse_break_stmt(Parser* p)
{
    return parse_control_stmt(p, KW_BREAK, stmt_break);
}

static Stmt* parse_continue_stmt(Parser* p)
{
    return parse_control_stmt(p, KW_CONTINUE, stmt_continue);
}

//
// Expression Parsing
//

typedef enum {
    PREC_NONE = 0,
    PREC_TERNARY,
    PREC_OR,
    PREC_AND,
    PREC_BIT_OR,
    PREC_BIT_XOR,
    PREC_BIT_AND,
    PREC_EQ,
    PREC_SHIFT,
    PREC_ADD,
    PREC_MUL,
    PREC_UNARY,
    PREC_DOT,
    PREC_CALL,
    PREC_PRIMARY,
} Precedence;

static int get_precedence(Token_Kind kind)
{
    switch ((int)kind) {
        case '?':       return PREC_TERNARY;

        case TOK_OR:    return PREC_OR;
        case TOK_AND:   return PREC_AND;
        case '|':       return PREC_BIT_OR;
        case '^':       return PREC_BIT_XOR;
        case '&':       return PREC_BIT_AND;

        case TOK_EQ:
        case TOK_NEQ:
        case '<':
        case TOK_LTEQ:
        case '>':
        case TOK_GTEQ:
            return PREC_EQ;

        case TOK_LSHIFT:
        case TOK_RSHIFT:
            return PREC_SHIFT;

        case '+':       return PREC_ADD;
        case '-':       return PREC_ADD;

        case '*':       return PREC_MUL;
        case '/':       return PREC_MUL;
        case '%':       return PREC_MUL;

        case '.':       return PREC_DOT;
        case '(':       return PREC_CALL;
    }

    return PREC_NONE;
}

static Expr* parse_expr_with_precedence(Parser* p, int precedence);
static Expr* parse_prefix_expr(Parser* p);
static Expr* parse_primary_expr(Parser* p);
static Expr* parse_struct_expr(Parser* p);
static Expr* parse_union_expr(Parser* p);
static Expr* parse_enum_expr(Parser* p);
static Expr* parse_function_expr(Parser* p);

static Expr* parse_expr_internal(Parser* p, bool allow_lists)
{
    Expr* expr = parse_expr_with_precedence(p, 0);
    if (!match(p, ',') || !allow_lists) {
        return expr;
    }

    // It's a list expression: `e1, e2, e3, ...`
    Expr** exprs = NULL;
    darray_add(exprs, expr);
    for (;;) {
        expr = parse_expr_with_precedence(p, 0);
        darray_add(exprs, expr);
        if (!match(p, ',')) { break; }
    }

    expr = expr_list(&p->ast_arena, exprs, darray_len(exprs));
    return expr;
}

static Expr* parse_expr(Parser* p)
{
    return parse_expr_internal(p, true);
}

static Expr* parse_single_expr(Parser* p)
{
    return parse_expr_internal(p, false);
}

static Expr* parse_expr_with_precedence(Parser* p, int precedence)
{
    Expr* left = parse_prefix_expr(p);
    if (!left) { return NULL; }

    for (;;) {
        if (p->current.kind == ':' || p->current.kind == '=' ||
            p->current.kind == TOK_VAR_ASSIGN || p->current.kind == TOK_CONST_ASSIGN)
        {
            break;
        }

        if (p->current.kind == '(') {
            // Function call
            advance(p); // Consume '('
            for (;;) {
            }
        }

        int curr_precedence = get_precedence(p->current.kind);
        // If we encounter any non-binary operator, we get a NONE precedence
        // and we can break out of the loop.
        if (curr_precedence == PREC_NONE || curr_precedence <= precedence) {
            break;
        }
        advance(p);
        Expr* right = parse_expr_with_precedence(p, curr_precedence);
        if (!right) {
            syntax_error("Expected expression after binary operator");
            return NULL;
        }
        left = expr_binary(&p->ast_arena, p->current.kind, left, right);
    }

    return left;
}

static Expr* parse_grouping_expr(Parser* p)
{
    consume(p, '(', "Expected '(' at start of grouping expression");
    Expr* expr = parse_expr(p);
    if (!expr) {
        syntax_error("Expected expression after '('");
        return NULL;
    }
    if (!consume(p, ')', "Expected ')' after expression")) {
        return NULL;
    }
    return expr;
}

static Expr* parse_prefix_expr(Parser* p)
{
    switch (p->current.kind) {
        default:
            syntax_error("Unexpected token type `%s` in expression", token_kind_to_string(p->current.kind));
            return NULL;

        case TOK_INT_LITERAL:
        case TOK_FLT_LITERAL:
        case TOK_CHAR_LITERAL:
        case TOK_STRING_LITERAL:
        case TOK_KEYWORD:
        case TOK_IDENTIFIER: {
            return parse_primary_expr(p);
        } break;

        case '(': return parse_grouping_expr(p); break;

        case '+':
        case '-':
        case '!':
        case '~': {
            advance(p);
            Expr* right = parse_expr_with_precedence(p, PREC_UNARY);
            if (!right) {
                syntax_error("Expected expression after unary operator");
                return NULL;
            }
            return expr_unary(&p->ast_arena, p->current.kind, right);
        } break;
    }
}

static Expr* parse_primary_expr(Parser* p)
{
    Expr* expr = NULL;
    switch (p->current.kind) {
        case TOK_INT_LITERAL: {
            expr = expr_int(&p->ast_arena, p->current.ivalue);
            advance(p);
        } break;
        case TOK_FLT_LITERAL: {
            expr = expr_float(&p->ast_arena, p->current.fvalue);
            advance(p);
        } break;
        case TOK_CHAR_LITERAL: {
            expr = expr_int(&p->ast_arena, p->current.cvalue); // @TODO: Flag chars
            advance(p);
        } break;
        case TOK_STRING_LITERAL: {
            expr = expr_str(&p->ast_arena, dup_string(&p->string_arena, p->current.svalue));
            advance(p);
        } break;
        case TOK_IDENTIFIER: {
            expr = expr_name(&p->ast_arena, dup_string(&p->string_arena, p->current.svalue));
            advance(p);
        } break;
        case TOK_KEYWORD: {
            switch (p->current.keyword) {
                case KW_TRUE:       expr = expr_bool(&p->ast_arena, true);  advance(p); break;
                case KW_FALSE:      expr = expr_bool(&p->ast_arena, false); advance(p); break;
                case KW_NULL:       expr = expr_null(&p->ast_arena);        advance(p); break;
                case KW_STRUCT:     expr = parse_struct_expr(p);    break;
                case KW_UNION:      expr = parse_union_expr(p);     break;
                case KW_ENUM:       expr = parse_enum_expr(p);      break;
                case KW_FN:         expr = parse_function_expr(p);  break;

                default:
                    syntax_error("Unexpected keyword `%s` in expression", keyword_to_string(p->current.keyword));
                    return NULL;
            }
        } break;

        default:
            syntax_error("Unexpected token in expression");
            return NULL;
    }

    return expr;
}

static Expr* parse_aggreagate_expr(Parser* p, Keyword kw, Expr* (*make_expr)(Arena*, Stmt**, int))
{
    if (!match_keyword(p, kw)) {
        ASSERT_ALWAYS("parse_struct_expr called but no 'struct' keyword found");
        return NULL;
    }

    consume(p, '{', "Expected '{' after 'struct'");

    Stmt** members = NULL;
    for (;;) {
        Stmt* member = parse_expr_stmt(p);
        if (!member) {
            syntax_error("Expected member declaration in struct expression");
            darray_free(members);
            return NULL;
        }

        darray_add(members, member);
        if (match(p, '}')) { break; }
    }

    Expr* expr = make_expr(&p->ast_arena, members, darray_len(members));
    darray_free(members);
    return expr;
}

static Expr* parse_struct_expr(Parser* p)
{
    return parse_aggreagate_expr(p, KW_STRUCT, expr_struct);
}

static Expr* parse_union_expr(Parser* p)
{
    return parse_aggreagate_expr(p, KW_UNION, expr_union);
}

static Expr* parse_enum_expr(Parser* p)
{
    if (!match_keyword(p, KW_ENUM)) {
        ASSERT_ALWAYS("parse_enum_expr called but no 'enum' keyword found");
        return NULL;
    }

    consume(p, '{', "Expected '{' after 'enum'");
    Enum_Item* enumerants = NULL;
    Stmt** members = NULL;

    for (;;) {
        if (match(p, '}')) { break; }
        if (p->current.kind != TOK_IDENTIFIER) {
            syntax_error("Expected identifier in enum declaration");
            darray_free(members);
            return NULL;
        }

        if (peek(p, 1).kind == '=' || peek(p, 1).kind == ',') {
            // Simple enum member
            String member_name = dup_string(&p->string_arena, p->current.svalue);
            advance(p);
            Expr* value_expr = NULL;
            if (match(p, '=')) {
                value_expr = parse_single_expr(p);
                if (!value_expr) {
                    syntax_error("Expected expression after '=' in enum member");
                    darray_free(members);
                    return NULL;
                }
            }
            match(p, ',');
            darray_add(enumerants, (Enum_Item){
                .name  = member_name,
                .value = value_expr,
            });
        }
        else {
            // Statement for a member
            Stmt* member_stmt = parse_expr_stmt(p);
            if (!member_stmt) {
                syntax_error("Expected enum member declaration");
                darray_free(members);
                return NULL;
            }
            darray_add(members, member_stmt);
            continue;
        }
    }

    Expr* expr = expr_enum(&p->ast_arena, enumerants, darray_len(enumerants), members, darray_len(members));
    darray_free(enumerants);
    darray_free(members);
    return expr;
}

static Expr* parse_function_expr(Parser* p)
{
    if (!match_keyword(p, KW_FN)) {
        ASSERT_ALWAYS("parse_function_expr called but no 'fn' keyword found");
        return NULL;
    }

    consume(p, '(', "Expected '(' after 'fn'");

    // Parse parameters
    Function_Param* params = NULL;
    for (;;) {
        if (match(p, ')')) { break; }
        if (p->current.kind != TOK_IDENTIFIER) {
            syntax_error("Expected parameter name in function declaration");
            darray_free(params);
            return NULL;
        }
        String param_name = dup_string(&p->string_arena, p->current.svalue);
        advance(p);
        if (!consume(p, ':', "Expected ':' after parameter name")) {
            darray_free(params);
            return NULL;
        }
        Expr* param_type = parse_single_expr(p);
        if (!param_type) {
            syntax_error("Expected parameter type in function declaration");
            darray_free(params);
            return NULL;
        }
        Expr* default_value = NULL;
        if (match(p, '=')) {
            default_value = parse_single_expr(p);
            if (!default_value) {
                syntax_error("Expected default value expression in function parameter");
                darray_free(params);
                return NULL;
            }
        }
        darray_add(params, (Function_Param){
            .name          = param_name,
            .type          = param_type,
            .default_value = default_value,
        });
        if (!match(p, ',')) {
            consume(p, ')', "Expected ')' after function parameters");
            break;
        }
    }

    // Parse return type
    Expr* return_type = NULL;
    if (!match(p, '{')) {
        return_type = parse_single_expr(p);
        if (!return_type) {
            syntax_error("Expected return type or '{' after function parameters");
            darray_free(params);
            return NULL;
        }
    }

    // Parse function body
    consume(p, '{', "Expected '{' at start of function body");
    Stmt** body_stmts = NULL;
    while (!parser_is_at_end(p) && p->current.kind != '}') {
        Stmt* body_stmt = parse_stmt(p);
        if (!body_stmt) {
            syntax_error("Expected statement in function body");
            darray_free(params);
            darray_free(body_stmts);
            return NULL;
        }
    }
    consume(p, '}', "Expected '}' at end of function body");
    Stmt* body = stmt_block(&p->ast_arena, body_stmts, darray_len(body_stmts), (String){0});
    darray_free(body_stmts);
    Expr* expr = expr_function(&p->ast_arena, params, darray_len(params), return_type, body);
    darray_free(params);
    return expr;
}

//
// Test
//

static void parse_and_print_decl(String input)
{
    printf("Parsing declaration: %.*s\n", (int)input.len, input.data);
    Parser parser = parser_init(input, NULL);
    Stmt* stmt = parser_next_stmt(&parser);
    ASSERT(stmt);
    stmt_print(stmt, 0);
    printf("\n");

    parser_shutdown(&parser);
}

static void parse_complex_code(void)
{
    String code = str_from_lit(
        "MyType :: struct {\n"
        "  x, y: f32;\n"
        "  label: [16]const u8;\n"
        "  is_active :bool= true;\n"
        "  is_visible := false;\n"
        "}\n"
        "\n"
        "make_point :: fn(x: f32, y: f32, label: [16]const u8) MyType {\n"
        "  res :MyType;\n"
        "  res.x = x;\n"
        "  res.y = y;\n"
        "  res.label = label;\n"
        "  return res;\n"
        "}\n"
        "\n"
        "main :: fn() s32 {\n"
        "  p := make_point(10.0, 20.0, \"Origin\");\n"
        "  if p.is_active {\n"
        "    trace(\"Point %s is at ({}, {})\", p.label, p.x, p.y);\n"
        "  } else {\n"
        "    trace(\"Point {} is inactive\", p.label);\n"
        "  }\n"
        "  print(\"Size of MyType: {} bytes\", sizeof(MyType));\n"
        "  return 0;\n"
        "}\n");

    Parser parser = parser_init(code, NULL);

    Stmt** stmts = NULL;
    while (!parser_is_at_end(&parser)) {
        Stmt* stmt = parser_next_stmt(&parser);
        if (stmt) {
            darray_add(stmts, stmt);
        }
        stmt_print(stmt, 0);
        printf("\n");
    }

    darray_free(stmts);
    parser_shutdown(&parser);
}

TEST(parse)
{
    break_on_syntax_error = true;

    parse_and_print_decl(str_from_lit("struct { x, y: f32; }"));
    parse_and_print_decl(str_from_lit("struct {\n  x, y: f32;\n  s := \"hello\";\n}"));
    parse_and_print_decl(str_from_lit("union { i: s32; f: f32; }"));
    parse_and_print_decl(str_from_lit("enum { Red, Green = 5, Blue, }"));
    parse_and_print_decl(
        str_from_lit("fn(n: s32) s32 { trace(\"fact\"); if n <= 1 { return 1; } return n * fact(n - 1); }"));

    parse_complex_code();
}

