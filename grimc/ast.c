#include "ast.h"

thread_local Arena* ast_arena;

void ast_set_arena(Arena* arena) {
    ast_arena = arena;
}

static void* ast_alloc(size_t size) {
    ASSERT(ast_arena != NULL, "AST arena is not set");
    return arena_alloc(ast_arena, size);
}

static void* ast_dup(const void* src, size_t size) {
    ASSERT(ast_arena != NULL, "AST arena is not set");
    void* dst = arena_alloc(ast_arena, size);
    memcpy(dst, src, size);
    return dst;
}

#define AST_DUP(ptr, count) ast_dup((ptr), sizeof(*(ptr)) * (size_t)(count))

//
// Node allocation
//

static Stmt* new_stmt(Stmt_Kind kind) {
    Stmt* stmt = ast_alloc(sizeof(Stmt));
    memset(stmt, 0, sizeof(Stmt));
    stmt->kind = kind;
    return stmt;
}

static Typespec* new_typespec(Typespec_Kind kind) {
    Typespec* ts = ast_alloc(sizeof(Typespec));
    memset(ts, 0, sizeof(Typespec));
    ts->kind = kind;
    return ts;
}

static Decl* new_decl(Decl_Kind kind) {
    Decl* decl = ast_alloc(sizeof(Decl));
    memset(decl, 0, sizeof(Decl));
    decl->kind = kind;
    return decl;
}

static Expr* new_expr(Expr_Kind kind) {
    Expr* expr = ast_alloc(sizeof(Expr));
    memset(expr, 0, sizeof(Expr));
    expr->kind = kind;
    return expr;
}

//
// Stmt
//

Stmt* stmt_assign(Token_Kind op, Expr* left, Expr* right) {
    Stmt* stmt = new_stmt(STMT_ASSIGN);
    stmt->assignment.op    = op;
    stmt->assignment.left  = left;
    stmt->assignment.right = right;
    return stmt;
}

static Stmt* new_stmt_decl(Stmt_Kind kind, Expr* left, Typespec* type, Expr* right) {
    Stmt* stmt = new_stmt(kind);
    stmt->decl.left  = left;
    stmt->decl.type  = type;
    stmt->decl.right = right;
    return stmt;
}

Stmt* stmt_decl_var(Expr* left, Typespec* type, Expr* right) {
    return new_stmt_decl(STMT_DECL_VAR, left, type, right);
}

Stmt* stmt_decl_const(Expr* left, Typespec* type, Expr* right) {
    return new_stmt_decl(STMT_DECL_CONST, left, type, right);
}

Stmt* stmt_expr(Expr* expr) {
    Stmt* stmt = new_stmt(STMT_EXPR);
    stmt->expr = expr;
    return stmt;
}

Stmt* stmt_return(Expr* expr) {
    Stmt* stmt = new_stmt(STMT_RETURN);
    stmt->expr = expr;
    return stmt;
}

Stmt* stmt_break(const char* label) {
    Stmt* stmt = new_stmt(STMT_BREAK);
    stmt->control.label = str_from_cstr(label);
    return stmt;
}

Stmt* stmt_continue(const char* label) {
    Stmt* stmt = new_stmt(STMT_BREAK);
    stmt->control.label = str_from_cstr(label);
    return stmt;
}

Stmt* stmt_block(Stmt** stmts, int stmt_count) {
    Stmt* stmt = new_stmt(STMT_BLOCK);
    stmt->block.stmts      = AST_DUP(stmts, stmt_count);
    stmt->block.stmt_count = stmt_count;
    return stmt;
}

Stmt* stmt_if(Expr* condition, Stmt* then_branch, Stmt* else_branch) {
    Stmt* stmt = new_stmt(STMT_IF);
    stmt->if_stmt.condition   = condition;
    stmt->if_stmt.then_branch = then_branch;
    stmt->if_stmt.else_branch = else_branch;
    return stmt;
}

Stmt* stmt_while(Expr* condition, Stmt* body) {
    Stmt* stmt = new_stmt(STMT_WHILE);
    stmt->while_stmt.condition = condition;
    stmt->while_stmt.body      = body;
    return stmt;
}

Stmt* stmt_do_while(Stmt* body, Expr* condition) {
    Stmt* stmt = new_stmt(STMT_DO);
    stmt->while_stmt.body      = body;
    stmt->while_stmt.condition = condition;
    return stmt;
}

//
// Typespec
//

Typespec* typespec_name(const char* name) {
    Typespec* ts = new_typespec(TYPESPEC_NAME);
    ts->name = name;
    return ts;
}

Typespec* typespec_array(Typespec* base, Expr* size, bool is_const) {
    Typespec* ts = new_typespec(TYPESPEC_ARRAY);
    ts->array.base     = base;
    ts->array.size     = size;
    ts->array.is_const = is_const;
    return ts;
}

Typespec* typespec_proc(Typespec** params, int param_count, Typespec* return_type) {
    Typespec* ts = new_typespec(TYPESPEC_PROC);
    ts->proc.return_type = return_type;
    ts->proc.params      = AST_DUP(params, param_count);
    ts->proc.param_count = param_count;
    return ts;
}

Typespec* typespec_pointer(Typespec* base, bool is_const) {
    Typespec* ts = new_typespec(TYPESPEC_POINTER);
    ts->pointer.base     = base;
    ts->pointer.is_const = is_const;
    return ts;
}

//
// Decl
//

Decl* decl_enum(Enum_Item* items, int item_count, Proc_Decl* methods, int method_count) {
    Decl* decl = new_decl(DECL_ENUM);
    decl->enum_decl.items        = AST_DUP(items, item_count);
    decl->enum_decl.item_count   = item_count;
    decl->enum_decl.methods      = methods;
    decl->enum_decl.method_count = method_count;
    return decl;
}

Decl* decl_struct(Aggregate_Item* items, int item_count, Proc_Decl* methods, int method_count) {
    Decl* decl = new_decl(DECL_STRUCT);
    decl->aggregate_decl.items        = AST_DUP(items, item_count);
    decl->aggregate_decl.item_count   = item_count;
    decl->aggregate_decl.methods      = methods;
    decl->aggregate_decl.method_count = method_count;
    return decl;
}

Decl* decl_union(Aggregate_Item* items, int item_count, Proc_Decl* methods, int method_count) {
    Decl* decl = new_decl(DECL_UNION);
    decl->aggregate_decl.items        = AST_DUP(items, item_count);
    decl->aggregate_decl.item_count   = item_count;
    decl->aggregate_decl.methods      = AST_DUP(methods, method_count);
    decl->aggregate_decl.method_count = method_count;
    return decl;
}

Decl* decl_proc(Aggregate_Item* args, int arg_count, Typespec* return_type, Stmt* body) {
    Decl* decl = new_decl(DECL_PROC);
    decl->proc_decl.args         = AST_DUP(args, arg_count);
    decl->proc_decl.arg_count    = arg_count;
    decl->proc_decl.return_type  = return_type;
    decl->proc_decl.body         = body;
    return decl;
}

//
// Expr
//

Expr* expr_list(Expr** exprs, int expr_count) {
    Expr* expr = new_expr(EXPR_LIST);
    expr->list.exprs      = AST_DUP(exprs, expr_count);
    expr->list.expr_count = expr_count;
    return expr;
}

Expr* expr_int(u64 value) {
    Expr* expr = new_expr(EXPR_INT);
    expr->ivalue = value;
    return expr;
}

Expr* expr_bool(bool value) {
    Expr* expr = new_expr(EXPR_BOOL);
    expr->bvalue = value;
    return expr;
}

Expr* expr_null(void) {
    Expr* expr = new_expr(EXPR_NULL);
    return expr;
}

Expr* expr_flt(double value) {
    Expr* expr = new_expr(EXPR_FLT);
    expr->fvalue = value;
    return expr;
}

Expr* expr_str(const char* str) {
    Expr* expr = new_expr(EXPR_STR);
    expr->svalue = str;
    return expr;
}

Expr* expr_name(const char* name) {
    Expr* expr = new_expr(EXPR_NAME);
    expr->name = str_intern(name);
    return expr;
}

Expr* expr_call(const char* name, Expr** args, int arg_count) {
    Expr* expr = new_expr(EXPR_CALL);
    expr->call.name      = name;
    expr->call.args      = AST_DUP(args, arg_count);
    expr->call.arg_count = arg_count;
    return expr;
}

Expr* expr_unary(Token_Kind op, Expr* operand) {
    Expr* expr = new_expr(EXPR_UNARY);
    expr->unary.op      = op;
    expr->unary.operand = operand;
    return expr;
}

Expr* expr_binary(Token_Kind op, Expr* left, Expr* right) {
    Expr* expr = new_expr(EXPR_BINARY);
    expr->binary.op    = op;
    expr->binary.left  = left;
    expr->binary.right = right;
    return expr;
}

Expr* expr_ternary(Expr* condition, Expr* then_expr, Expr* else_expr) {
    Expr* expr = new_expr(EXPR_TERNARY);
    expr->ternary.condition = condition;
    expr->ternary.then_expr = then_expr;
    expr->ternary.else_expr = else_expr;
    return expr;
}

Expr* expr_cast(Typespec* type, Expr* expr) {
    Expr* e = new_expr(EXPR_CAST);
    e->cast.type = type;
    e->cast.expr = expr;
    return e;
}

Expr* expr_index(Expr* expr, Expr* index) {
    Expr* e = new_expr(EXPR_INDEX);
    e->index.expr  = expr;
    e->index.index = index;
    return e;
}

Expr* expr_compound(Typespec* type, Compound_Initializer* initializers, int initializer_count) {
    Expr* expr = new_expr(EXPR_COMPOUND);
    expr->compound.type              = type;
    expr->compound.initalizers       = AST_DUP(initializers, initializer_count);
    expr->compound.initializer_count = initializer_count;
    return expr;
}

Expr* expr_decl(Decl* decl) {
    Expr* expr = new_expr(EXPR_DECL);
    expr->decl = decl;
    return expr;
}

Expr* expr_sizeof_expr(Expr* expr) {
    Expr* e = new_expr(EXPR_SIZEOF_EXPR);
    e->sizeof_expr = expr;
    return e;
}

Expr* expr_sizeof_type(Typespec* type) {
    Expr* e = new_expr(EXPR_SIZEOF_TYPE);
    e->sizeof_type = type;
    return e;
}

Expr* expr_alignof_expr(Expr* expr) {
    Expr* e = new_expr(EXPR_ALIGNOF_EXPR);
    e->alignof_expr = expr;
    return e;
}

Expr* expr_alignof_type(Typespec* type) {
    Expr* e = new_expr(EXPR_ALIGNOF_TYPE);
    e->alignof_type = type;
    return e;
}

//
// Print functions
//

void print_stmt(Stmt* stmt, int indent) {
    ASSERT(stmt);
    printf("%*s", indent, "");

    switch (stmt->kind) {
        case STMT_NONE: ASSERT_ALWAYS("STMT_NONE"); break;

        case STMT_ASSIGN:
            printf("(%s ", token_kind_to_string(stmt->assignment.op));
            print_expr(stmt->assignment.left, 0);
            print_expr(stmt->assignment.right, 0);
            printf(")");
            break;

        case STMT_DECL_VAR:
        case STMT_DECL_CONST:
            if (stmt->kind == STMT_DECL_VAR) {
                printf("(var ");
            } else {
                printf("(const ");
            }
            print_expr(stmt->decl.left, 0);
            if (stmt->decl.type) {
                printf(" :");
                print_typespec(stmt->decl.type, 0);
            }
            if (stmt->decl.right) {
                printf(" ");
                print_expr(stmt->decl.right, 0);
            }
            printf(")");
            break;

        case STMT_EXPR:
            print_expr(stmt->expr, 0);
            break;

        case STMT_RETURN:
            printf("(return");
            if (stmt->expr) {
                print_expr(stmt->expr, 1);
            }
            printf(")");
            break;

        case STMT_BREAK:
            if (stmt->control.label.len > 0) {
                printf("(break %.*s)", stmt->control.label.len, stmt->control.label.data);
            } else {
                printf("(break)");
            }
            break;

        case STMT_CONTINUE:
            if (stmt->control.label.len > 0) {
                printf("(continue %.*s)", stmt->control.label.len, stmt->control.label.data);
            } else {
                printf("(continue)");
            }
            break;

        case STMT_BLOCK:
            printf("(block\n");
            for (int i = 0; i < stmt->block.stmt_count; i++) {
                if (i != 0) { printf("\n"); }
                print_stmt(stmt->block.stmts[i], indent + 2);
            }
            printf(")");
            break;

        case STMT_IF:
            printf("(if ");
            print_expr(stmt->if_stmt.condition, 0);
            printf("\n");
            print_stmt(stmt->if_stmt.then_branch, indent + 4);
            if (stmt->if_stmt.else_branch) {
                printf("\n%*selse\n", indent + 2, "");
                print_stmt(stmt->if_stmt.else_branch, indent + 4);
            }
            printf("\n%*s)", indent, "");
            break;

        case STMT_WHILE:
            printf("(while ");
            print_expr(stmt->while_stmt.condition, 0);
            printf("\n");
            print_stmt(stmt->while_stmt.body, indent + 2);
            printf("%*s)", indent, "");
            break;

        case STMT_DO:
            printf("(do\n");
            print_stmt(stmt->while_stmt.body, indent + 4);
            printf("\n%*swhile ", indent + 2, "");
            print_expr(stmt->while_stmt.condition, 0);
            printf("%*s)", indent, "");
            break;

        case STMT_SWITCH:
            printf("(switch\n");
            for (int i = 0; i < stmt->switch_stmt.case_count; i++) {
                Case_Branch* branch = stmt->switch_stmt.cases[i];
                printf("%*scase ", indent + 2, "");
                for (int j = 0; j < branch->value_count; j++) {
                    if (j != 0) {
                        printf(", ");
                    }
                    print_expr(branch->values[j], 0);
                }
                printf(":\n");
                print_stmt(branch->body, indent + 4);
                printf("\n");
            }
            if (stmt->switch_stmt.default_case) {
                printf("%*sdefault:\n", indent + 2, "");
                print_stmt(stmt->switch_stmt.default_case, indent + 4);
                printf("\n");
            }
            printf("%*s)", indent, "");
            break;

        default: ASSERT_ALWAYS("Unknown Stmt_Kind"); break;
    }
}

void print_typespec(Typespec* type, int indent) {
    ASSERT(type);
    printf("%*s", indent, "");
    switch (type->kind) {
        case TYPESPEC_NONE: ASSERT_ALWAYS("TYPESPEC_NONE"); break;
        case TYPESPEC_NAME:
            printf("%s", type->name);
            break;

        case TYPESPEC_ARRAY:
            printf("[");
            if (type->array.size) {
                print_expr(type->array.size, 0);
            }
            printf("]");
            if (type->array.is_const) {
                printf(" const ");
            }
            print_typespec(type->array.base, 0);
            break;

        case TYPESPEC_POINTER:
            printf("*");
            if (type->pointer.is_const) {
                printf(" const ");
            }
            print_typespec(type->pointer.base, 0);
            break;

        case TYPESPEC_PROC:
            printf("(proc-type");
            for (int i = 0; i < type->proc.param_count; i++) {
                if (i != 0) {
                    printf(", ");
                }
                print_typespec(type->proc.params[i], 0);
            }
            if (type->proc.return_type) {
                printf(" -> ");
                print_typespec(type->proc.return_type, 0);
            }
            printf(")");
            break;

    }
}

static void print_aggregate_item(Aggregate_Item* item, int indent) {
    ASSERT(item);
    printf("%*s", indent, "");
    for (int j = 0; j < item->names_count; j++) {
        if (j != 0) {
            printf(", ");
        }
        printf("%.*s", item->names[j].len, item->names[j].data);
    }
    printf(": ");
    if (item->type)
        print_typespec(item->type, 0);
    if (item->default_value) {
        if (item->type) {
            printf(" = ");
        } else {
            printf(":= ");
        }
        print_expr(item->default_value, 0);
    }
}

void print_decl(Decl* decl, int indent) {
    ASSERT(decl);
    printf("%*s", indent, "");
    switch (decl->kind) {
        case DECL_NONE: ASSERT_ALWAYS("DECL_NONE"); break;

        case DECL_ENUM: {
            printf("(enum\n");
            for (int i = 0; i < decl->enum_decl.item_count; i++) {
                if (i != 0) { printf("\n"); }
                Enum_Item* item = &decl->enum_decl.items[i];
                printf("%*s%.*s", indent + 2, "", item->name.len, item->name.data);
                if (item->value) {
                    printf(" = ");
                    print_expr(item->value, 0);
                }
            }
            for (int i = 0; i < decl->enum_decl.method_count; i++) {
                if (i != 0 || decl->enum_decl.item_count == 0) { printf("\n"); }
                print_decl((Decl*)&decl->enum_decl.methods[i], indent + 2);
            }
            printf(")");

        } break;

        case DECL_STRUCT:
        case DECL_UNION: {
            const char* kind_str = decl->kind == DECL_STRUCT ? "struct" : "union";
            printf("(%s\n", kind_str);
            for (int i = 0; i < decl->aggregate_decl.item_count; i++) {
                if (i != 0) { printf("\n"); }
                print_aggregate_item(&decl->aggregate_decl.items[i], indent + 2);
            }
            for (int i = 0; i < decl->aggregate_decl.method_count; i++) {
                if (i != 0 || decl->aggregate_decl.item_count == 0) { printf("\n"); }
                print_decl((Decl*)&decl->aggregate_decl.methods[i], indent + 2);
            }
            printf(")");
        } break;

        case DECL_PROC: {
            printf("(proc (");
            for (int i = 0; i < decl->proc_decl.arg_count; i++) {
                if (i != 0) { printf(", "); }
                print_aggregate_item(&decl->proc_decl.args[i], 0);
            }
            printf(")");
            if (decl->proc_decl.return_type) {
                printf(" -> ");
                print_typespec(decl->proc_decl.return_type, 0);
            }
            if (decl->proc_decl.body) {
                printf("\n");
                print_stmt(decl->proc_decl.body, indent + 2);
            }
            printf(")");
        } break;
    }
}

void print_expr(Expr* expr, int indent) {
    ASSERT(expr);
    printf("%*s", indent, "");
    switch (expr->kind) {
        case EXPR_NONE:
            ASSERT_ALWAYS("EXPR_NONE");
            break;

        case EXPR_LIST:
            printf("(list");
            for (int i = 0; i < expr->list.expr_count; i++) {
                printf(" ");
                print_expr(expr->list.exprs[i], 0);
            }
            printf(")");
            break;

        case EXPR_INT:
            printf("%llu", expr->ivalue);
            break;

        case EXPR_BOOL:
            printf(expr->bvalue ? "true" : "false");
            break;

        case EXPR_NULL:
            printf("null");
            break;

        case EXPR_FLT:
            printf("%f", expr->fvalue);
            break;

        case EXPR_STR:
            // @TODO: Escape special characters
            printf("\"%s\"", expr->svalue);
            break;

        case EXPR_NAME:
            printf("%s", expr->name);
            break;

        case EXPR_CALL:
            printf("(%s", expr->call.name);
            for (int i = 0; expr->call.args && i < expr->call.arg_count; i++) {
                printf(" ");
                print_expr(expr->call.args[i], 0);
            }
            printf(")");
            break;

        case EXPR_UNARY:
            printf("(%s ", token_kind_to_string(expr->unary.op));
            print_expr(expr->unary.operand, 0);
            printf(")");
            break;

        case EXPR_BINARY:
            printf("(%s ", token_kind_to_string(expr->binary.op));
            print_expr(expr->binary.left, 0);
            printf(" ");
            print_expr(expr->binary.right, 0);
            printf(")");
            break;

        case EXPR_TERNARY:
            printf("(?: ");
            print_expr(expr->ternary.condition, 0);
            printf(" ");
            print_expr(expr->ternary.then_expr, 0);
            printf(" ");
            print_expr(expr->ternary.else_expr, 0);
            printf(")");
            break;

        case EXPR_CAST:
            printf("((cast ");
            print_typespec(expr->cast.type, 0);
            printf(" ");
            print_expr(expr->cast.expr, 0);
            printf(")");
            break;

        case EXPR_DECL:
            print_decl(expr->decl, 0);
            break;

        case EXPR_SIZEOF_EXPR:
            printf("(sizeof_expr ");
            print_expr(expr->sizeof_expr, 0);
            printf(")");
            break;

        case EXPR_SIZEOF_TYPE:
            printf("(sizeof_type ");
            print_typespec(expr->sizeof_type, 0);
            printf(")");
            break;

        case EXPR_ALIGNOF_EXPR:
            printf("(alignof_expr ");
            print_expr(expr->alignof_expr, 0);
            printf(")");
            break;

        case EXPR_ALIGNOF_TYPE:
            printf("(alignof_type ");
            print_typespec(expr->alignof_type, 0);
            printf(")");
            break;

        default: ASSERT_ALWAYS("Unhandled expr kind %d", expr->kind); break;
    }
}

//
//
//

static void test_expr(void) {
    Expr* exprs[] = {
        expr_int(123),
        expr_flt(3.14),
        expr_str("hello"),
        expr_name("variable"),
        expr_unary('-', expr_int(42)),
        expr_binary('+', expr_int(1), expr_int(2)),
        expr_ternary(
            expr_binary(TOK_LTEQ, expr_name("x"), expr_int(10)),
            expr_str("less than 10"),
            expr_str("10 or more")),
        expr_cast(new_typespec(TYPESPEC_NAME), expr_int(100)),
        expr_call("my_function", &(Expr*[]){
                expr_int(1),
                expr_int(2),
                expr_flt(3.0),
                expr_str("test"),
                expr_name("foo"),
            }[0], 5),
    };

    ASSERT(exprs[0]->kind == EXPR_INT, "Expected EXPR_INT, got %d", exprs[0]->kind);
    ASSERT(exprs[0]->ivalue == 123, "Expected 123, got %llu", exprs[0]->ivalue);
    ASSERT(exprs[1]->kind == EXPR_FLT, "Expected EXPR_FLOAT, got %d", exprs[1]->kind);
    ASSERT(fabs(exprs[1]->fvalue - 3.14) < 0.000001, "Expected 3.14, got %f", exprs[1]->fvalue);
    ASSERT(exprs[2]->kind == EXPR_STR, "Expected EXPR_STR, got %d", exprs[2]->kind);
    ASSERT(strcmp(exprs[2]->svalue, "hello") == 0,
        "Expected 'hello', got '%s'", exprs[2]->svalue);
    ASSERT(exprs[3]->kind == EXPR_NAME, "Expected EXPR_NAME, got %d", exprs[3]->kind);
    ASSERT(strcmp(exprs[3]->name, "variable") == 0,
        "Expected 'variable', got '%s'", exprs[3]->name);
    ASSERT(exprs[4]->kind == EXPR_UNARY, "Expected EXPR_UNARY, got %d", exprs[4]->kind);
    ASSERT(exprs[4]->unary.op == '-', "Expected TOK_SUB, got %d", exprs[4]->unary.op);
    ASSERT(exprs[4]->unary.operand->kind == EXPR_INT,
        "Expected EXPR_INT, got %d", exprs[4]->unary.operand->kind);
    ASSERT(exprs[4]->unary.operand->ivalue == 42,
        "Expected 42, got %llu", exprs[4]->unary.operand->ivalue);

    printf("Expressions:\n");
    for (size_t i = 0; i < COUNTOF(exprs); i++) {
        print_expr(exprs[i], 2);
        printf("\n");
    }
}

static void test_typespec(void) {
    Typespec* types[] = {
        typespec_name("s32"),
        typespec_pointer(typespec_name("u8"), false),
        typespec_array(typespec_name("f32"), expr_int(10), true),
        typespec_proc(
            (Typespec*[]){
                typespec_name("s32"),
                typespec_pointer(typespec_name("u8"), true),
            },
            2,
            NULL),
        typespec_proc(
            (Typespec*[]){ typespec_name("Param_Type"), },
            1,
            typespec_name("bool")),
    };

    ASSERT(types[0]->kind == TYPESPEC_NAME, "Expected TYPESPEC_NAME, got %d", types[0]->kind);
    ASSERT(strcmp(types[0]->name, "s32") == 0, "Expected 's32', got '%s'", types[0]->name);
    ASSERT(types[1]->kind == TYPESPEC_POINTER, "Expected TYPESPEC_POINTER, got %d", types[1]->kind);
    ASSERT(types[1]->pointer.base->kind == TYPESPEC_NAME, "Expected TYPESPEC_NAME, got %d", types[1]->pointer.base->kind);
    ASSERT(strcmp(types[1]->pointer.base->name, "u8") == 0, "Expected 'u8', got '%s'", types[1]->pointer.base->name);
    ASSERT(types[1]->pointer.is_const == false, "Expected is_const false, got true");
    ASSERT(types[2]->kind == TYPESPEC_ARRAY, "Expected TYPESPEC_ARRAY, got %d", types[2]->kind);
    ASSERT(types[2]->array.base->kind == TYPESPEC_NAME, "Expected TYPESPEC_NAME, got %d", types[2]->array.base->kind);
    ASSERT(strcmp(types[2]->array.base->name, "f32") == 0, "Expected 'f32', got '%s'", types[2]->array.base->name);
    ASSERT(types[2]->array.size->kind == EXPR_INT, "Expected EXPR_INT, got %d", types[2]->array.size->kind);
    ASSERT(types[2]->array.size->ivalue == 10, "Expected size 10, got %llu", types[2]->array.size->ivalue);
    ASSERT(types[3]->kind == TYPESPEC_PROC, "Expected TYPESPEC_PROC, got %d", types[3]->kind);
    ASSERT(types[3]->proc.param_count == 2, "Expected 2 params, got %d", types[3]->proc.param_count);
    ASSERT(types[3]->proc.params[0]->kind == TYPESPEC_NAME, "Expected TYPESPEC_NAME, got %d", types[3]->proc.params[0]->kind);
    ASSERT(strcmp(types[3]->proc.params[0]->name, "s32") == 0, "Expected 's32', got '%s'", types[3]->proc.params[0]->name);
    ASSERT(types[3]->proc.params[1]->kind == TYPESPEC_POINTER, "Expected TYPESPEC_POINTER, got %d",
        types[3]->proc.params[1]->kind);
    ASSERT(types[3]->proc.params[1]->pointer.base->kind == TYPESPEC_NAME,
        "Expected TYPESPEC_NAME, got %d", types[3]->proc.params[1]->pointer.base->kind);
    ASSERT(strcmp(types[3]->proc.params[1]->pointer.base->name, "u8") == 0,
        "Expected 'u8', got '%s'", types[3]->proc.params[1]->pointer.base->name);
    ASSERT(types[4]->kind == TYPESPEC_PROC, "Expected TYPESPEC_PROC, got %d", types[4]->kind);
    ASSERT(types[4]->proc.param_count == 1, "Expected 1 param, got %d", types[4]->proc.param_count);
    ASSERT(types[4]->proc.params[0]->kind == TYPESPEC_NAME,
        "Expected TYPESPEC_NAME, got %d", types[4]->proc.params[0]->kind);
    ASSERT(strcmp(types[4]->proc.params[0]->name, "Param_Type") == 0,
        "Expected 'Param_Type', got '%s'", types[4]->proc.params[0]->name);
    ASSERT(types[4]->proc.return_type->kind == TYPESPEC_NAME,
        "Expected TYPESPEC_NAME, got %d", types[4]->proc.return_type->kind);
    ASSERT(strcmp(types[4]->proc.return_type->name, "bool") == 0,
        "Expected 'bool', got '%s'", types[4]->proc.return_type->name);

    printf("\nTypespecs:\n");
    for (size_t i = 0; i < COUNTOF(types); i++) {
        print_typespec(types[i], 2);
        printf("\n");
    }
}

TEST(ast) {
    Arena test_arena = {0};
    ast_set_arena(&test_arena);
    test_expr();
    test_typespec();
    arena_reset(&test_arena);
}

