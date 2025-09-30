#include "parse.h"
#include "lex.h"
#include "ast.h"

static int          get_precedence(Token_Kind kind);
static void         advance(Parse_Context* p);

static Decl*        parse_proc_decl(Parse_Context* p);
static Decl*        parse_struct_or_union_decl(Parse_Context* p);
static Decl*        parse_enum_decl(Parse_Context* p);

static bool         parse_function_arg(Parse_Context* p, Aggregate_Item* out_item);
static bool         try_parse_aggregate_item(Parse_Context* p, Aggregate_Item* out_item);

static Typespec*    parse_typespec(Parse_Context* p);

static Expr*        parse_expr_precedence(Parse_Context* p, bool is_lhs, int precedence);

static Stmt*        parse_block_stmt(Parse_Context* p);

static bool         match_token(Parse_Context* p, Token_Kind kind);
static bool         match_keyword(Parse_Context* p, Keyword kw);
static bool         is_token(Parse_Context* p, Token_Kind kind);
static bool         is_keyword(Parse_Context* p, Keyword kw);

Parse_Context parse_init(const char* source) {
    Parse_Context res = (Parse_Context){ .lexer = lexer_init(source) };
    return res;
}

void parse_shutdown(Parse_Context* pctx) {
    arena_reset(&pctx->ast_arena);
}

void parse_begin(Parse_Context* p) {
    p->previous = (Token){0};
    p->current  = lexer_next_token(&p->lexer);
    ast_set_arena(&p->ast_arena);
}

void parse_end(Parse_Context* p) {
    ast_set_arena(NULL);
}

static const int UNARY_PRECEDENCE = 20;
static const int CALL_PRECEDENCE  = 21;

static int get_precedence(Token_Kind kind) {
    switch ((int)kind) {
        case '?':       return 1;

        case TOK_OR:    return 2;
        case TOK_AND:   return 3;
        case '|':       return 4;
        case '^':       return 5;
        case '&':       return 6;

        case TOK_EQ:
        case TOK_NEQ:
        case '<':
        case TOK_LTEQ:
        case '>':
        case TOK_GTEQ:
            return 7;

        case TOK_LSHIFT:
        case TOK_RSHIFT:
            return 8;

        case '+':       return  9;
        case '-':       return  9;

        case '*':       return 10;
        case '/':       return 10;
        case '%':       return 10;

        case '.':       return 11;
    }

    return 0;
}

static bool is_binary_op(Parse_Context* p) {
    return get_precedence(p->current.kind) != 0;
}


static void advance(Parse_Context* p) {
    if (p->current.kind == TOK_EOF) { return; }
    p->previous = p->current;
    // @TODO: Handle token errors
    p->current  = lexer_next_token(&p->lexer);
}

PRINTF_LIKE(2, 0)
static void parse_errorv(Parse_Context* p, const char* fmt, va_list args) {
    char static_buffer[2048];
    char* buffer = static_buffer;
    int written = vsnprintf(buffer, sizeof(static_buffer), fmt, args);
    if (written < 0) {
        fprintf(stderr, "Error formatting parse_errorv() message\n");
        exit(1);
    }
    if ((usize)written >= sizeof(buffer)) {
        buffer = xmalloc((usize)written + 1);
        written = vsnprintf(buffer, (usize)written + 1, fmt, args);
        if (written < 0) {
            fprintf(stderr, "Error formatting parse_errorv() message\n");
            exit(1);
        }
    }

    syntax_error("%s", buffer);

    if (buffer != static_buffer) {
        free(buffer);
    }
}

PRINTF_LIKE(3, 4)
static void consume(Parse_Context* p, Token_Kind kind, const char *err_fmt, ...) {
    if (p->current.kind == kind) {
        advance(p);
        return;
    }

    va_list args;
    va_start(args, err_fmt);
    parse_errorv(p, err_fmt, args);
    va_end(args);
}

PRINTF_LIKE(3, 4)
static void consume_keyword(Parse_Context* p, Keyword kw, const char *err_fmt, ...) {
    if (p->current.kind == TOK_KEYWORD && p->current.keyword == kw) {
        advance(p);
        return;
    }

    va_list args;
    va_start(args, err_fmt);
    parse_errorv(p, err_fmt, args);
    va_end(args);
}

PRINTF_LIKE(4, 5)
static void consume_keywords(Parse_Context* p, Keyword* kw, int kw_count, const char *err_fmt, ...) {
    if (p->current.kind == TOK_KEYWORD) {
        for (int i = 0; i < kw_count; i++) {
            if (p->current.keyword == kw[i]) {
                advance(p);
                return;
            }
        }
    }

    va_list args;
    va_start(args, err_fmt);
    parse_errorv(p, err_fmt, args);
    va_end(args);
}

//
// Declaration
//

Decl* parse_decl(Parse_Context* p) {
    if (is_token(p, TOK_KEYWORD)) {
        switch (p->current.keyword) {
            case KW_PROC:
                return parse_proc_decl(p);
            case KW_STRUCT:
            case KW_UNION:
                return parse_struct_or_union_decl(p);
            case KW_ENUM:
                return parse_enum_decl(p);
            default:
                return NULL;
        }
    }
    return NULL;
}


static Decl* parse_proc_decl(Parse_Context* p) {
    consume_keyword(p, KW_PROC, "Expected 'proc' keyword");

    // @TODO: Parse function specifiers?

    consume(p, '(', "Expected '(' after 'proc'");
    Aggregate_Item* args = NULL;
    for (;;) {
        if (is_token(p, ')')) { break; }
        if (match_token(p, TOK_EOF)) {
            syntax_error("Unexpected end of file in function arguments");
            break;
        }

        Aggregate_Item item;
        if (parse_function_arg(p, &item)) {
            darray_add(args, item);
        } else {
            syntax_error("Expected function argument");
            break;
        }
        if (match_token(p, ',')) { continue; }

        break;
    }
    consume(p, ')', "Expected ')' after function arguments");
    Typespec* return_type = NULL;
    if (!is_token(p, '{')) {
        return_type = parse_typespec(p);
        if (!return_type) {
            syntax_error("Expected return type or function body");
            return NULL;
        }
    }
    Stmt* body = parse_block_stmt(p);
    return decl_proc(args, darray_len(args), return_type, body);
}

static Decl* parse_struct_or_union_decl(Parse_Context* p) {
    consume_keywords(p, (Keyword[]){KW_STRUCT, KW_UNION}, 2, "Expected 'struct' or 'union' keyword");
    bool is_struct = p->previous.keyword == KW_STRUCT;

    consume(p, '{', "Expected '{' after %s", is_struct ? "struct" : "union");
    Aggregate_Item* items = NULL;
    Proc_Decl* methods = NULL;

    for (;;) {
        if (is_token(p, '}')) { break; }
        if (is_token(p, TOK_EOF)) {
            syntax_error("Unexpected end of file in %s declaration", is_struct ? "struct" : "union");
            break;
        }

        Aggregate_Item item;
        if (try_parse_aggregate_item(p, &item)) {
            darray_add(items, item);
        } else {
            syntax_error("Expected struct/union member declaration");
            break;
        }
        consume(p, ';', "Expected ';' after struct/union member declaration");

        // @TODO: Parse member functions
    }

    consume(p, '}', "Expected '}' after %s body", is_struct ? "struct" : "union");

    return is_struct
        ? decl_struct(items, darray_len(items), methods, darray_len(methods))
        : decl_union(items, darray_len(items), methods, darray_len(methods));

}

static Decl* parse_enum_decl(Parse_Context* p) {
    consume_keyword(p, KW_ENUM, "Expected 'enum' keyword");
    consume(p, '{', "Expected '{' after 'enum'");

    Enum_Item* items = NULL;
    Proc_Decl* methods = NULL;

    for (;;) {
        if (p->current.kind == '}') { break; }
        if (p->current.kind == TOK_EOF) {
            syntax_error("Unexpected end of file in enum declaration");
            break;
        }

        consume(p, TOK_IDENTIFIER, "Expected enum item name");
        Token name = p->previous;

        Expr* value = NULL;
        if (match_token(p, '=')) {
            value = parse_expr(p);
            if (!value) {
                syntax_error("Expected enum item value expression");
                return NULL;
            }
        }

        Enum_Item item = {
            .name  = str_from_cstr(name.name),
            .value = value,
        };
        darray_add(items, item);

        if (!match_token(p, ',')) { break; }
    }

    consume(p, '}', "Expected '}' after enum body");
    return decl_enum(items, darray_len(items), methods, darray_len(methods));
}

static bool parse_function_arg(Parse_Context* p, Aggregate_Item* out_item) {
    // Maybe it will need to be different, but for now just use the same
    // as aggregate item parsing.
    return try_parse_aggregate_item(p, out_item);
}

static bool try_parse_aggregate_item(Parse_Context* p, Aggregate_Item* out_item) {
    String* names = NULL;

    for (;;) {
        if (p->current.kind != TOK_IDENTIFIER) { break; }

        darray_add(names, str_from_cstr(p->current.name));
        advance(p);

        if (!match_token(p, ',')) { break; }
    }

    if (names == NULL) {
        return false;
    }

    Typespec* type = NULL;
    Expr* default_value = NULL;

    if (match_token(p, TOK_VAR_ASSIGN)) {
        default_value = parse_expr(p);
        if (!default_value) {
            syntax_error("Expected expression after ':='");
            return false;
        }
    }
    else if (match_token(p, ':')) {
        type = parse_typespec(p);
        if (!type) {
            syntax_error("Expected type in aggregate item");
            return false;
        }

        if (match_token(p, '=')) {
            default_value = parse_expr(p);
            if (!default_value) {
                syntax_error("Expected expression after '='");
                return false;
            }
        }
    } else {
        syntax_error("Expected type or default value in aggregate item");
        return false;
    }

    out_item->names         = names;
    out_item->names_count   = (int)darray_len(names);
    out_item->type          = type;
    out_item->default_value = default_value;
    return true;
}

//
// Typespec
//

static Typespec* parse_typespec(Parse_Context* p) {
    advance(p);
    switch (p->previous.kind) {
        case '[': {
            Expr* size_expr = is_token(p, ']') ? NULL : parse_expr(p);
            consume(p, ']', "Expected ']'");
            bool is_const = match_keyword(p, KW_CONST);
            Typespec* element_type = parse_typespec(p);
            if (!element_type) {
                syntax_error("Expected element type in array typespec");
                return NULL;
            }
            return typespec_array(element_type, size_expr, is_const);
        }

        case '*': {
            bool is_const = match_keyword(p, KW_CONST);
            Typespec* base_type = parse_typespec(p);
            if (!base_type) {
                syntax_error("Expected base type in pointer typespec");
                return NULL;
            }
            return typespec_pointer(base_type, is_const);
        } break;

        case TOK_IDENTIFIER: {
            return typespec_name(p->previous.name);
        } break;

        default:
            syntax_error("Unexpected token in typespec: %s", token_kind_to_string(p->previous.kind));
            break;
    }

    return NULL;
}

//
// Expression
//

static Expr* parse_unary_expr(Parse_Context* p, bool is_lhs) {
    advance(p);
    switch (p->previous.kind) {
        case '+':
        case '-':
        case '!':
        case '~':
        case '*':
        case TOK_INCREMENT:
        case TOK_DECREMENT:
            return expr_unary(p->previous.kind, parse_expr_precedence(p, is_lhs, UNARY_PRECEDENCE));

        case '(': {
            Expr* inner = parse_expr_precedence(p, is_lhs, 0);
            consume(p, ')', "Expected ')' after expression");
            return inner;
        }

        case TOK_INT_LITERAL:
            return expr_int(p->previous.ivalue);
        case TOK_FLT_LITERAL:
            return expr_flt(p->previous.fvalue);
        case TOK_CHAR_LITERAL:
            NOT_IMPLEMENTED();  // @TODO: Needs to be marked as a char so that we can properly convert in the
                                // C back-end.
            return expr_int((u64)p->previous.cvalue);
        case TOK_STRING_LITERAL:
            return expr_str(p->previous.svalue);
        case TOK_IDENTIFIER:
            return expr_name(p->previous.name);

        case TOK_KEYWORD: {
            switch (p->previous.keyword) {
                case KW_SIZEOF:
                case KW_ALIGNOF: {
                    bool is_sizeof = p->previous.keyword == KW_SIZEOF;
                    consume(p, '(', "Expected '(' after 'sizeof'");
                    if (match_token(p, ':')) {
                        Typespec* type = parse_typespec(p);
                        if (!type) {
                            syntax_error("Expected type in 'sizeof' expression");
                            return NULL;
                        }
                        consume(p, ')', "Expected ')' after 'sizeof' type");
                        return is_sizeof
                            ? expr_sizeof_type(type)
                            : expr_alignof_type(type);
                    } else {
                        Expr* expr = parse_expr(p);
                        if (!expr) {
                            syntax_error("Expected expression in 'sizeof' expression");
                            return NULL;
                        }
                        consume(p, ')', "Expected ')' after 'sizeof' expression");
                        return is_sizeof
                            ? expr_sizeof_expr(expr)
                            : expr_alignof_expr(expr);
                    }
                };

                case KW_TRUE:
                    return expr_bool(true);
                case KW_FALSE:
                    return expr_bool(false);
                case KW_NULL:
                    return expr_null();

                case KW_CAST: {
                    consume(p, '(', "Expected '(' after 'cast'");
                    Typespec* target_type = parse_typespec(p);
                    if (!target_type) {
                        syntax_error("Expected target type in 'cast' expression");
                        return NULL;
                    }
                    consume(p, ')', "Expected ')' after 'cast' target type");
                    Expr* value = parse_expr_precedence(p, is_lhs, UNARY_PRECEDENCE);
                    if (!value) {
                        syntax_error("Expected value expression in 'cast' expression");
                        return NULL;
                    }
                    return expr_cast(target_type, value);
                } break;

                default:
                    syntax_error("Unexpected keyword in expression: %s", keyword_to_string(p->previous.keyword));
                    return NULL;
            }
        } break;

        default:
            break;
    }

    return NULL;
}

static Expr* parse_expr_precedence(Parse_Context* p, bool is_lhs, int min_prec) {
    Expr* left = parse_unary_expr(p, is_lhs);
    if (!left) { return NULL; }

    for (;;) {
        if (is_token(p, ',') || is_token(p, ';')) { break; }

        if (match_token(p, '(')) {
            Expr** args = NULL;
            for (;;) {
                if (is_token(p, ')')) { break; }
                Expr* arg = parse_expr(p);
                if (!arg) {
                    syntax_error("Expected function argument expression");
                    return NULL;
                }
                darray_add(args, arg);
                if (!match_token(p, ',')) { break; }
            }
            consume(p, ')', "Expected ')' after function call arguments");
            left = expr_call(left->name, args, darray_len(args));
            continue;
        }

        if (!is_binary_op(p)) { break; }

        int prec = get_precedence(p->current.kind);
        ASSERT(prec != 0, "Expected non-zero precedence for binary operator");

        if (prec < min_prec) { break; }

        Token_Kind op = p->current.kind;
        advance(p);

        Expr* right = parse_expr_precedence(p, is_lhs, prec + 1);
        if (!right) {
            syntax_error("Expected right-hand side expression");
            return NULL;
        }

        left = expr_binary(op, left, right);
    }

    return left;
}

Expr* parse_expr(Parse_Context* p) {
    Decl* decl = parse_decl(p);
    if (decl) {
        return expr_decl(decl);
    }

    return parse_expr_precedence(p, false, 0);
}

static Expr* parse_list_expr(Parse_Context* p) {
    Expr** elements = NULL;
    for (;;) {
        Expr* elem = parse_expr(p);
        if (!elem)
            break;
        darray_add(elements, elem);
        if (!match_token(p, ',')) {
            break;
        }
    }
    if (darray_len(elements) == 0) {
         return NULL;
    }
    if (darray_len(elements) == 1) {
        Expr* single = elements[0];
        darray_free(elements);
        return single;
    }

    return expr_list(elements, darray_len(elements));
}

//
// Statement
//

static Stmt* parse_expr_or_assignment_stmt(Parse_Context* p) {
    Expr* lhs_list = parse_list_expr(p);
    if (!lhs_list) {
        return NULL;
    }

    switch (p->current.kind) {
        case '=':
        case TOK_ADD_ASSIGN:
        case TOK_SUB_ASSIGN:
        case TOK_MUL_ASSIGN:
        case TOK_DIV_ASSIGN:
        case TOK_MOD_ASSIGN:
        case TOK_AND_ASSIGN:
        case TOK_OR_ASSIGN:
        case TOK_XOR_ASSIGN:
        case TOK_LSHIFT_ASSIGN:
        case TOK_RSHIFT_ASSIGN:
        case TOK_BIT_AND_ASSIGN:
        case TOK_BIT_OR_ASSIGN: {
            Token_Kind assign_op = p->current.kind;
            advance(p);
            Expr* rhs_list = parse_list_expr(p);
            if (!rhs_list) {
                syntax_error("Expected right-hand side expression in assignment");
                return NULL;
            }
            consume(p, ';', "Expected ';' after assignment statement");
            return stmt_assign(assign_op, lhs_list, rhs_list);
        }

        case TOK_VAR_ASSIGN:
        case TOK_CONST_ASSIGN: {
            bool is_const = p->current.kind == TOK_CONST_ASSIGN;
            advance(p);
            Expr* rhs_list = parse_list_expr(p);
            if (!rhs_list) {
                syntax_error("Expected right-hand side expression in declaration statement");
                return NULL;
            }
            bool can_skip_semicolon =
                (rhs_list->kind == EXPR_DECL) ||
                (rhs_list->kind == EXPR_LIST && rhs_list->list.exprs[rhs_list->list.expr_count - 1]->kind == EXPR_DECL);

            if (!can_skip_semicolon)
                consume(p, ';', "Expected ';' after declaration statement");

            if (is_const) {
                return stmt_decl_const(lhs_list, NULL, rhs_list);
            } else {
                return stmt_decl_var(lhs_list, NULL, rhs_list);
            }
        }

        case ':': {
            advance(p);
            Typespec* type = parse_typespec(p);
            if (!type) {
                syntax_error("Expected type in declaration statement");
                return NULL;
            }

            if (match_token(p, ';')) {
                return stmt_decl_var(lhs_list, type, NULL);
            }

            advance(p);
            bool is_const = false;
            switch (p->previous.kind) {
                case ':': is_const = true;  break;
                case '=': is_const = false; break;
                default:
                    syntax_error("Expected ':', ':=', or '=' in declaration statement");
                    return NULL;
            }

            Expr* rhs_list = parse_list_expr(p);
            if (!rhs_list) {
                syntax_error("Expected right-hand side expression in declaration statement");
                return NULL;
            }

            bool can_skip_semicolon =
                (rhs_list->kind == EXPR_DECL) ||
                (rhs_list->kind == EXPR_LIST && rhs_list->list.exprs[rhs_list->list.expr_count - 1]->kind == EXPR_DECL);
            if (!can_skip_semicolon)
                consume(p, ';', "Expected ';' after declaration statement");

            if (is_const) {
                return stmt_decl_const(lhs_list, type, rhs_list);
            } else {
                return stmt_decl_var(lhs_list, type, rhs_list);
            }
        } break;

        default:
            return stmt_expr(lhs_list);
    }
    UNREACHABLE("Unhandled case in parse_expr_or_assignment_stmt");
}

Stmt* parse_stmt(Parse_Context* p) {
    switch (p->current.kind) {
        case '{':
            return parse_block_stmt(p);

        case TOK_KEYWORD: {
            advance(p);
            switch (p->previous.keyword) {
                case KW_BREAK: {
                } break;

                case KW_CONTINUE: {
                } break;

                case KW_IF: {
                    Expr* condition = parse_expr(p);
                    if (!condition) {
                        syntax_error("Expected condition expression in 'if' statement");
                        return NULL;
                    }
                    Stmt* then_branch = parse_stmt(p);
                    if (!then_branch) {
                        syntax_error("Expected 'then' branch statement in 'if' statement");
                        return NULL;
                    }
                    Stmt* else_branch = NULL;
                    if (match_keyword(p, KW_ELSE)) {
                        else_branch = parse_stmt(p);
                        if (!else_branch) {
                            syntax_error("Expected 'else' branch statement in 'if' statement");
                            return NULL;
                        }
                    }
                    return stmt_if(condition, then_branch, else_branch);
                } break;

                case KW_RETURN: {
                    Expr* value = NULL;
                    if (!is_token(p, ';') && !is_token(p, '}')) {
                        value = parse_expr(p);
                        if (!value) {
                            syntax_error("Expected return value expression");
                            return NULL;
                        }
                    }
                    return stmt_return(value);
                } break;

                case KW_WHILE: {
                    Expr* condition = parse_expr(p);
                    if (!condition) {
                        syntax_error("Expected condition expression in 'while' statement");
                        return NULL;
                    }
                    Stmt* body = parse_stmt(p);
                    if (!body) {
                        syntax_error("Expected body statement in 'while' statement");
                        return NULL;
                    }
                    return stmt_while(condition, body);
                } break;

                case KW_DO: {
                    Stmt* body = parse_stmt(p);
                    if (!body) {
                        syntax_error("Expected body statement in 'do' statement");
                        return NULL;
                    }
                    consume_keyword(p, KW_WHILE, "Expected 'while' after 'do' body");
                    Expr* condition = parse_expr(p);
                    if (!condition) {
                        syntax_error("Expected condition expression in 'do...while' statement");
                        return NULL;
                    }
                    consume(p, ';', "Expected ';' after 'do...while' condition");
                    return stmt_do_while(body, condition);
                } break;

                case KW_FOR: {
                    NOT_IMPLEMENTED();
                } break;

                case KW_SWITCH: {
                    NOT_IMPLEMENTED();
                } break;

                default:
                    syntax_error("Unexpected keyword in statement: %s", keyword_to_string(p->previous.keyword));
                    return NULL;
            }
        } break;

        default: {
            return parse_expr_or_assignment_stmt(p);
        } break;
    }

    return NULL;
}

static Stmt* parse_block_stmt(Parse_Context* p) {
    if (!match_token(p, '{')) {
        syntax_error("Expected '{' to start block statement");
        return NULL;
    }

    Stmt** stmts = NULL;
    for (;;) {
        if (is_token(p, '}')) { break; }
        if (is_token(p, TOK_EOF)) {
            syntax_error("Unexpected end of file in block statement");
            break;
        }
        Stmt* stmt = parse_stmt(p);
        if (stmt) {
            darray_add(stmts, stmt);
        }
    }
    consume(p, '}', "Expected '}' to end block statement");
    return stmt_block(stmts, darray_len(stmts));
}

//
// Helpers
//

static bool match_token(Parse_Context* p, Token_Kind kind) {
    if (!is_token(p, kind)) { return false; }
    advance(p);
    return true;
}

static bool match_keyword(Parse_Context* p, Keyword kw) {
    if (!is_keyword(p, kw)) { return false; }
    advance(p);
    return true;
}

static bool is_token(Parse_Context* p, Token_Kind kind) {
    return p->current.kind == kind;
}

static bool is_keyword(Parse_Context* p, Keyword kw) {
    return p->current.kind == TOK_KEYWORD && p->current.keyword == kw;
}

//
// Test
//

static void parse_and_print_decl(const char* input) {
    Parse_Context pctx = parse_init(input);
    parse_begin(&pctx);
    Decl* decl = parse_decl(&pctx);
    if (decl) {
        print_decl(decl, 0);
        printf("\n");
    } else {
        ASSERT_ALWAYS("Failed to parse declaration");
    }
    parse_end(&pctx);
    parse_shutdown(&pctx);
}

static void parse_complex_code(void) {
    const char* code =
        "MyType :: struct {\n"
        "  x, y: f32;\n"
        "  label: [16]const u8;\n"
        "  is_active :bool= true;\n"
        "  is_visible := false;\n"
        "};\n"
        "\n"
        "make_point :: proc(x: f32, y: f32, label: [16]const u8) MyType {\n"
        "  res :MyType;\n"
        "  res.x = x;\n"
        "  res.y = y;\n"
        "  res.label = label;\n"
        "  return res;\n"
        "}\n"
        "\n"
        "main :: proc() s32 {\n"
        "  p := make_point(10.0, 20.0, \"Origin\");\n"
        "  if p.is_active {\n"
        "    trace(\"Point %s is at ({}, {})\", p.label, p.x, p.y);\n"
        "  } else {\n"
        "    trace(\"Point {} is inactive\", p.label);\n"
        "  }\n"
        "  print(\"Size of MyType: {} bytes\", sizeof(:MyType));\n"
        "  return 0;\n"
        "}\n";

    Parse_Context pctx = parse_init(code);
    parse_begin(&pctx);

    Stmt** stmts = NULL;
    while (pctx.current.kind != TOK_EOF) {
        Stmt* stmt = parse_stmt(&pctx);
        if (stmt) {
            darray_add(stmts, stmt);
        }
    }

    for (Stmt** it = stmts; it != darray_end(stmts); it++) {
        print_stmt(*it, 0);
        printf("\n");
    }

    darray_free(stmts);
    parse_end(&pctx);
    parse_shutdown(&pctx);
}

TEST(parse) {
    break_on_syntax_error = true;
    parse_complex_code();

    parse_and_print_decl("struct { x, y: f32; }");
    parse_and_print_decl("struct {\n  x, y: f32;\n  s := \"hello\";\n}");
    parse_and_print_decl("union { i: s32; f: f32; }");
    parse_and_print_decl("enum { Red, Green = 5, Blue, }");
    parse_and_print_decl("proc(n: s32) s32 { trace(\"fact\"); if n <= 1 { return 1; } return n * fact(n - 1); }");
}

