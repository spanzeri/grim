#include "ast.h"

//
// Statements
//

static Stmt* ast_alloc_stmt(Arena* arena, Stmt_Kind kind, usize size)
{
    Stmt* stmt = memset(arena_alloc(arena, size), 0, size);
    stmt->kind = kind;
    return stmt;
}

#define STMT_ALLOC(arena, Type, kind)   ((Type*)ast_alloc_stmt(arena, kind, sizeof(Type)))

Stmt* stmt_assignment(Arena* arena, Token_Kind op, Expr* left, Expr* right)
{
    Assignment_Stmt* stmt = STMT_ALLOC(arena, Assignment_Stmt, STMT_ASSIGNMENT);
    stmt->op    = op;
    stmt->left  = left;
    stmt->right = right;
    return &stmt->base;
}

Stmt* stmt_decl_var(Arena* arena, Expr* left, Expr* typespec, Expr* right)
{
    Decl_Stmt* stmt = STMT_ALLOC(arena, Decl_Stmt, STMT_DECL_VAR);
    stmt->left     = left;
    stmt->typespec = typespec;
    stmt->right    = right;
    return &stmt->base;
}

Stmt* stmt_decl_const(Arena* arena, Expr* left, Expr* typespec, Expr* right)
{
    Decl_Stmt* stmt = STMT_ALLOC(arena, Decl_Stmt, STMT_DECL_CONST);
    stmt->left     = left;
    stmt->typespec = typespec;
    stmt->right    = right;
    return &stmt->base;
}

Stmt* stmt_expr(Arena* arena, Expr* expr)
{
    Expr_Stmt* stmt = STMT_ALLOC(arena, Expr_Stmt, STMT_EXPR);
    stmt->expr = expr;
    return &stmt->base;
}

Stmt* stmt_return(Arena* arena, Expr* expr)
{
    Return_Stmt* stmt = STMT_ALLOC(arena, Return_Stmt, STMT_RETURN);
    stmt->expr = expr;
    return &stmt->base;
}

Stmt* stmt_break(Arena* arena, String label)
{
    Control_Stmt* stmt = STMT_ALLOC(arena, Control_Stmt, STMT_CONTROL_BREAK);
    stmt->label = label;
    return &stmt->base;
}

Stmt* stmt_continue(Arena* arena, String label)
{
    Control_Stmt* stmt = STMT_ALLOC(arena, Control_Stmt, STMT_CONTROL_CONTINUE);
    stmt->label = label;
    return &stmt->base;
}

Stmt* stmt_switch(Arena* arena, Switch_Case* cases, int case_count, Stmt* default_case)
{
    Switch_Stmt* stmt = STMT_ALLOC(arena, Switch_Stmt, STMT_SWITCH);
    stmt->cases        = arena_dup_array(arena, cases, case_count);
    stmt->case_count   = case_count;
    stmt->default_case = default_case;
    return &stmt->base;
}

Stmt* stmt_block(Arena* arena, Stmt** stmts, int stmt_count, String label)
{
    Block_Stmt* stmt = STMT_ALLOC(arena, Block_Stmt, STMT_BLOCK);
    stmt->stmts      = arena_dup_array(arena, stmts, stmt_count);
    stmt->stmt_count = stmt_count;
    stmt->label      = label;
    return &stmt->base;
}

Stmt* stmt_if(Arena* arena, Expr* condition, Stmt* then_branch, Stmt* else_branch)
{
    return stmt_if_init(arena, NULL, condition, then_branch, else_branch);
}

Stmt* stmt_if_init(Arena* arena, Expr* init, Expr* condition, Stmt* then_branch, Stmt* else_branch)
{
    If_Stmt* stmt = STMT_ALLOC(arena, If_Stmt, STMT_IF);
    stmt->init        = init;
    stmt->condition   = condition;
    stmt->then_branch = then_branch;
    stmt->else_branch = else_branch;
    return &stmt->base;
}

Stmt* stmt_for(Arena* arena, Stmt* init, Expr* condition, Stmt* post, Stmt* body, String label)
{
    For_Stmt* stmt = STMT_ALLOC(arena, For_Stmt, STMT_FOR);
    stmt->init      = init;
    stmt->condition = condition;
    stmt->post      = post;
    stmt->body      = body;
    stmt->label     = label;
    return &stmt->base;
}

Stmt* stmt_range_for(Arena* arena, Expr* iterator, Expr* iterable, Stmt* body, String label)
{
    Range_For_Stmt* stmt = STMT_ALLOC(arena, Range_For_Stmt, STMT_RANGE_FOR);
    stmt->iterator = iterator;
    stmt->iterable = iterable;
    stmt->body     = body;
    stmt->label    = label;
    return &stmt->base;
}

void stmt_print(Stmt* stmt, int indent)
{
    ASSERT(stmt);

    printf("%*s", indent, "");

    switch (stmt->kind) {
        case STMT_ASSIGNMENT: {
            Assignment_Stmt* s = (Assignment_Stmt*)stmt;
            printf("(%s ", token_kind_to_string(s->op));
            expr_print(s->left, 0);
            expr_print(s->right, 0);
            printf(")");
        } break;

        case STMT_DECL_VAR: {
            Decl_Stmt* s = (Decl_Stmt*)stmt;
            printf("(%s ", stmt->kind == STMT_DECL_VAR ? "var" : "const");
            expr_print(s->left, 0);
            if (s->typespec) {
                printf(" :");
                expr_print(s->typespec, 0);
            }
            if (s->right) {
                printf(" ");
                expr_print(s->right, 0);
            }
            printf(")");
        } break;

        case STMT_EXPR: {
            Expr_Stmt* s = (Expr_Stmt*)stmt;
            expr_print(s->expr, 0);
        } break;

        case STMT_RETURN: {
            Return_Stmt* s = (Return_Stmt*)stmt;
            printf("(return");
            if (s->expr) {
                expr_print(s->expr, 1);
            }
            printf(")");
        } break;

        case STMT_CONTROL_BREAK:
        case STMT_CONTROL_CONTINUE: {
            Control_Stmt* s = (Control_Stmt*)stmt;
            printf("(%s", stmt->kind == STMT_CONTROL_BREAK ? "break" : "continue");
            if (!str_is_empty(s->label)) {
                printf(" %.*s", STR_FMT(s->label));
            }
            printf(")");
        } break;

        case STMT_IF: {
            If_Stmt* s = (If_Stmt*)stmt;
            printf("(if");
            if (s->init) {
                printf(" :init ");
                expr_print(s->init, 0);
            }
            printf(" ");
            expr_print(s->condition, 0);
            printf("\n");
            stmt_print(s->then_branch, indent + 4);
            if (s->else_branch) {
                printf("\n%*s:else\n", indent + 2, "");
                stmt_print(s->else_branch, indent + 4);
            }
            printf("\n%*s)", indent, "");
        } break;

        case STMT_BLOCK: {
            Block_Stmt* s = (Block_Stmt*)stmt;
            printf("(block\n");
            for (int i = 0; i < s->stmt_count; i++) {
                if (i != 0) { printf("\n"); }
                stmt_print(s->stmts[i], indent + 2);
            }
            printf(")");
        } break;

        case STMT_FOR: {
            For_Stmt* s = (For_Stmt*)stmt;
            printf("(for");
            if (!str_is_empty(s->label)) {
                printf(" %.*s: ", STR_FMT(s->label));
            }
            if (s->init) {
                printf(" :init ");
                stmt_print(s->init, 0);
            }
            if (s->condition) {
                printf(" :cond ");
                expr_print(s->condition, 0);
            }
            if (s->post) {
                printf(" :post ");
                stmt_print(s->post, 0);
            }
            printf("\n");
            stmt_print(s->body, indent + 2);
            printf("%*s)", indent, "");
        } break;

        case STMT_RANGE_FOR: {
            Range_For_Stmt* s = (Range_For_Stmt*)stmt;
            printf("(range_for");
            if (!str_is_empty(s->label)) {
                printf(" %.*s: ", STR_FMT(s->label));
            }
            printf(" :iter ");
            expr_print(s->iterator, 0);
            printf(" :in ");
            expr_print(s->iterable, 0);
            printf("\n");
            stmt_print(s->body, indent + 2);
            printf("%*s)", indent, "");
        } break;

        default: ASSERT_ALWAYS("Unknown Stmt_Kind: %d", stmt->kind); break;
    }
}

//
// Expr
//

static Expr* ast_alloc_expr(Arena* arena, Expr_Kind kind, usize size)
{
    Expr* expr = memset(arena_alloc(arena, size), 0, size);
    expr->kind = kind;
    return expr;
}

#define EXPR_ALLOC(arena, Type, kind)   ((Type*)ast_alloc_expr(arena, kind, sizeof(Type)))

Expr* expr_list(Arena* arena, Expr** exprs, int expr_count)
{
    List_Expr* expr = EXPR_ALLOC(arena, List_Expr, EXPR_LIST);
    expr->exprs      = arena_dup_array(arena, exprs, expr_count);
    expr->expr_count = expr_count;
    return &expr->base;
}

Expr* expr_int(Arena* arena, u64 value)
{
    Int_Expr* expr = EXPR_ALLOC(arena, Int_Expr, EXPR_INT);
    expr->value = value;
    return &expr->base;
}

Expr* expr_bool(Arena* arena, bool value)
{
    Bool_Expr* expr = EXPR_ALLOC(arena, Bool_Expr, EXPR_BOOL);
    expr->value = value;
    return &expr->base;
}

Expr* expr_null(Arena* arena)
{
    Null_Expr* expr = EXPR_ALLOC(arena, Null_Expr, EXPR_NULL);
    return &expr->base;
}

Expr* expr_float(Arena* arena, double value)
{
    Float_Expr* expr = EXPR_ALLOC(arena, Float_Expr, EXPR_FLOAT);
    expr->value = value;
    return &expr->base;
}

Expr* expr_str(Arena* arena, String value)
{
    String_Expr* expr = EXPR_ALLOC(arena, String_Expr, EXPR_STRING);
    expr->value = value;
    return &expr->base;
}

Expr* expr_name(Arena* arena, String name)
{
    Name_Expr* expr = EXPR_ALLOC(arena, Name_Expr, EXPR_NAME);
    expr->name = name;
    return &expr->base;
}

Expr* expr_call(Arena* arena, String name, Expr** args, int arg_count)
{
    Call_Expr* expr = EXPR_ALLOC(arena, Call_Expr, EXPR_CALL);
    expr->name      = name;
    expr->args      = arena_dup_array(arena, args, arg_count);
    expr->arg_count = arg_count;
    return &expr->base;
}

Expr* expr_unary(Arena* arena, Token_Kind op, Expr* operand)
{
    Unary_Expr* expr = EXPR_ALLOC(arena, Unary_Expr, EXPR_UNARY);
    expr->op      = op;
    expr->operand = operand;
    return &expr->base;
}

Expr* expr_binary(Arena* arena, Token_Kind op, Expr* left, Expr* right)
{
    Binary_Expr* expr = EXPR_ALLOC(arena, Binary_Expr, EXPR_BINARY);
    expr->op    = op;
    expr->left  = left;
    expr->right = right;
    return &expr->base;
}

Expr* expr_ternary(Arena* arena, Expr* condition, Expr* then_expr, Expr* else_expr)
{
    Ternary_Expr* expr = EXPR_ALLOC(arena, Ternary_Expr, EXPR_TERNARY);
    expr->condition  = condition;
    expr->then_expr  = then_expr;
    expr->else_expr  = else_expr;
    return &expr->base;
}

Expr* expr_cast(Arena* arena, Expr* type_expr, Expr* value_expr)
{
    Cast_Expr* expr = EXPR_ALLOC(arena, Cast_Expr, EXPR_CAST);
    expr->type_expr  = type_expr;
    expr->value_expr = value_expr;
    return &expr->base;
}

Expr* expr_index(Arena* arena, Expr* arr_expr, Expr* index)
{
    Index_Expr* expr = EXPR_ALLOC(arena, Index_Expr, EXPR_INDEX);
    expr->expr  = arr_expr;
    expr->index = index;
    return &expr->base;
}

Expr* expr_sizeof(Arena* arena, Expr* e)
{
    Type_Operator_Expr* expr = EXPR_ALLOC(arena, Type_Operator_Expr, EXPR_SIZEOF);
    expr->expr = e;
    return &expr->base;
}

Expr* expr_alignof(Arena* arena, Expr* e)
{
    Type_Operator_Expr* expr = EXPR_ALLOC(arena, Type_Operator_Expr, EXPR_ALIGNOF);
    expr->expr = e;
    return &expr->base;
}

Expr* expr_typeof(Arena* arena, Expr* e)
{
    Type_Operator_Expr* expr = EXPR_ALLOC(arena, Type_Operator_Expr, EXPR_TYPEOF);
    expr->expr = e;
    return &expr->base;
}

Expr* expr_pointer_type(Arena* arena, Expr* pointed_type)
{
    Pointer_Type_Expr* expr = EXPR_ALLOC(arena, Pointer_Type_Expr, EXPR_POINTER_TYPE);
    expr->pointed_type = pointed_type;
    return &expr->base;
}

Expr* expr_array_type(Arena* arena, Expr* element_type, Expr* size_expr)
{
    Array_Type_Expr* expr = EXPR_ALLOC(arena, Array_Type_Expr, EXPR_ARRAY_TYPE);
    expr->element_type  = element_type;
    expr->size_expr     = size_expr;
    return &expr->base;
}

Expr* expr_function(Arena* arena, Expr** param_types, int param_count, Expr* return_type, Stmt* body)
{
    Function_Expr* expr = EXPR_ALLOC(arena, Function_Expr, EXPR_FUNCTION);
    expr->param_types = arena_dup_array(arena, param_types, param_count);
    expr->param_count = param_count;
    expr->return_type = return_type;
    expr->body        = body;
    return &expr->base;
}

Expr* expr_struct(Arena* arena, Stmt** members, int member_count)
{
    Aggregate_Type_Expr* expr = EXPR_ALLOC(arena, Aggregate_Type_Expr, EXPR_AGGREGATE_STRUCT);
    expr->members      = arena_dup_array(arena, members, member_count);
    expr->member_count = member_count;
    return &expr->base;
}

Expr* expr_union(Arena* arena, Stmt** members, int member_count)
{
    Aggregate_Type_Expr* expr = EXPR_ALLOC(arena, Aggregate_Type_Expr, EXPR_AGGREGATE_UNION);
    expr->members      = arena_dup_array(arena, members, member_count);
    expr->member_count = member_count;
    return &expr->base;
}

Expr* expr_enum(Arena* arena, Enum_Item* items, int item_count, Function_Expr* methods, int method_count)
{
    Enum_Expr* expr = EXPR_ALLOC(arena, Enum_Expr, EXPR_ENUM);
    expr->items        = arena_dup_array(arena, items, item_count);
    expr->item_count   = item_count;
    expr->methods      = methods;
    expr->method_count = method_count;
    return &expr->base;
}

void expr_print(Expr* expr, int indent) {
    ASSERT(expr);
    printf("%*s", indent, "");
    switch (expr->kind) {
        case EXPR_LIST: {
            List_Expr* e = (List_Expr*)expr;
            printf("'(");
            for (int i = 0; i < e->expr_count; i++) {
                if (i != 0) { printf(" "); }
                expr_print(e->exprs[i], 0);
            }
            printf(")");
        } break;

        case EXPR_INT: {
            Int_Expr* e = (Int_Expr*)expr;
            printf("%llu", e->value);
        } break;

        case EXPR_FLOAT: {
            Float_Expr* e = (Float_Expr*)expr;
            printf("%f", e->value);
        } break;

        case EXPR_BOOL: {
            Bool_Expr* e = (Bool_Expr*)expr;
            printf("%s", e->value ? "true" : "false");
        } break;

        case EXPR_NULL: {
            printf("null");
        } break;

        case EXPR_STRING: {
            String_Expr* e = (String_Expr*)expr;
            printf("\"%.*s\"", STR_FMT(e->value));
        } break;

        case EXPR_NAME: {
            Name_Expr* e = (Name_Expr*)expr;
            printf("%.*s", STR_FMT(e->name));
        } break;

        case EXPR_CALL: {
            Call_Expr* e = (Call_Expr*)expr;
            printf("(%.*s", STR_FMT(e->name));
            for (int i = 0; i < e->arg_count; i++) {
                printf(" ");
                expr_print(e->args[i], 0);
            }
            printf(")");
        } break;

        case EXPR_UNARY: {
            Unary_Expr* e = (Unary_Expr*)expr;
            printf("(%s ", token_kind_to_string(e->op));
            expr_print(e->operand, 0);
            printf(")");
        } break;

        case EXPR_BINARY: {
            Binary_Expr* e = (Binary_Expr*)expr;
            printf("(%s ", token_kind_to_string(e->op));
            expr_print(e->left, 0);
            printf(" ");
            expr_print(e->right, 0);
            printf(")");
        } break;

        case EXPR_TERNARY: {
            Ternary_Expr* e = (Ternary_Expr*)expr;
            printf("(?: ");
            expr_print(e->condition, 0);
            printf(" ");
            expr_print(e->then_expr, 0);
            printf(" ");
            expr_print(e->else_expr, 0);
            printf(")");
        } break;

        case EXPR_CAST: {
            Cast_Expr* e = (Cast_Expr*)expr;
            printf("(cast ");
            expr_print(e->type_expr, 0);
            printf(" ");
            expr_print(e->value_expr, 0);
            printf(")");
        } break;

        case EXPR_INDEX: {
            Index_Expr* e = (Index_Expr*)expr;
            printf("([] ");
            expr_print(e->expr, 0);
            printf(" ");
            expr_print(e->index, 0);
            printf(")");
        } break;

        case EXPR_SIZEOF:
        case EXPR_ALIGNOF:
        case EXPR_TYPEOF: {
            Type_Operator_Expr* e = (Type_Operator_Expr*)expr;
            const char* op_str = expr->kind == EXPR_SIZEOF ? "sizeof" :
                                 expr->kind == EXPR_ALIGNOF ? "alignof" : "typeof";
            printf("(%s ", op_str);
            expr_print(e->expr, 0);
            printf(")");
        } break;

        case EXPR_POINTER_TYPE: {
            Pointer_Type_Expr* e = (Pointer_Type_Expr*)expr;
            printf("(ptr ");
            expr_print(e->pointed_type, 0);
            printf(")");
        } break;

        case EXPR_ARRAY_TYPE: {
            Array_Type_Expr* e = (Array_Type_Expr*)expr;
            printf("(array ");
            expr_print(e->element_type, 0);
            printf(" ");
            expr_print(e->size_expr, 0);
            printf(")");
        } break;

        case EXPR_FUNCTION: {
            Function_Expr* e = (Function_Expr*)expr;
            printf("(func");
            for (int i = 0; i < e->param_count; i++) {
                printf(" ");
                expr_print(e->param_types[i], 0);
            }
            if (e->return_type) {
                printf(" -> ");
                expr_print(e->return_type, 0);
            }
            if (e->body) {
                printf("\n");
                stmt_print(e->body, indent + 2);
                printf("\n%*s", indent, "");
            }
            printf(")");
        } break;

        case EXPR_AGGREGATE_STRUCT:
        case EXPR_AGGREGATE_UNION: {
            Aggregate_Type_Expr* e = (Aggregate_Type_Expr*)expr;
            const char* agg_str = expr->kind == EXPR_AGGREGATE_STRUCT ? "struct" : "union";
            printf("(%s\n", agg_str);
            for (int i = 0; i < e->member_count; i++) {
                printf("%*s", indent + 2, "");
                stmt_print(e->members[i], indent + 2);
                printf("\n");
            }
            printf("%*s)", indent, "");
        } break;

        case EXPR_ENUM: {
            Enum_Expr* e = (Enum_Expr*)expr;
            printf("(enum\n");
            for (int i = 0; i < e->item_count; i++) {
                Enum_Item* item = &e->items[i];
                printf("%*s%.*s", indent + 2, "", STR_FMT(item->name));
                if (item->value) {
                    printf(" = ");
                    expr_print(item->value, 0);
                }
                printf("\n");
            }
            for (int i = 0; i < e->method_count; i++) {
                printf("%*s", indent + 2, "");
                expr_print(&e->methods[i].base, indent + 2);
                printf("\n");
            }
            printf("%*s)", indent, "");
        } break;

        default: ASSERT_ALWAYS("Unknown Expr_Kind"); break;
    }
}

TEST(ast)
{
    Arena* arena = &(Arena){0};
    Expr* exprs[] = {
        expr_int(arena, 123),
        expr_float(arena, 3.14),
        expr_str(arena, str_from_cstr("hello")),
        expr_name(arena, str_from_cstr("variable")),
        expr_unary(arena, '-', expr_int(arena, 42)),
        expr_binary(arena, '+', expr_int(arena, 1), expr_int(arena, 2)),
        expr_ternary(arena,
            expr_binary(arena, TOK_LTEQ, expr_name(arena, str_from_cstr("x")), expr_int(arena, 10)),
            expr_str(arena, str_from_cstr("less than 10")),
            expr_str(arena, str_from_cstr("10 or more"))),
        expr_cast(arena, expr_pointer_type(arena, expr_name(arena, str_from_cstr("int"))), expr_int(arena, 100)),
        expr_call(arena, str_from_cstr("my_function"), &(Expr*[]){
                expr_int(arena, 1),
                expr_int(arena, 2),
                expr_float(arena, 3.0),
                expr_str(arena, str_from_cstr("test")),
                expr_name(arena, str_from_cstr("foo")),
            }[0], 5),
    };

    ASSERT(exprs[0]->kind == EXPR_INT, "Expected EXPR_INT, got %d", exprs[0]->kind);
    Int_Expr* iexpr = (Int_Expr*)exprs[0];
    ASSERT(iexpr->value = 123, "Expected 123, got %llu", iexpr->value);
    ASSERT(exprs[1]->kind == EXPR_FLOAT, "Expected EXPR_FLOAT, got %d", exprs[1]->kind);
    Float_Expr* fexpr = (Float_Expr*)exprs[1];
    ASSERT(fabs(fexpr->value - 3.14) < 0.0001, "Expected 3.14, got %f", fexpr->value);
    ASSERT(exprs[2]->kind == EXPR_STRING, "Expected EXPR_STR, got %d", exprs[2]->kind);
    String_Expr* sexpr = (String_Expr*)exprs[2];
    ASSERT(str_eq(sexpr->value, str_from_cstr("hello")), "Expected 'hello', got '%.*s'", STR_FMT(sexpr->value));
    ASSERT(exprs[3]->kind == EXPR_NAME, "Expected EXPR_NAME, got %d", exprs[3]->kind);
    Name_Expr* nexpr = (Name_Expr*)exprs[3];
    ASSERT(str_eq(nexpr->name, str_from_cstr("variable")), "Expected 'variable', got '%.*s'", STR_FMT(nexpr->name));
    ASSERT(exprs[4]->kind == EXPR_UNARY, "Expected EXPR_UNARY, got %d", exprs[4]->kind);
    Unary_Expr* uexpr = (Unary_Expr*)exprs[4];
    ASSERT(uexpr->op == '-', "Expected TOK_SUB, got %d", uexpr->op);
    ASSERT(uexpr->operand->kind == EXPR_INT, "Expected EXPR_INT, got %d", uexpr->operand->kind);
    ASSERT(((Int_Expr*)uexpr->operand)->value == 42, "Expected 42, got %llu", ((Int_Expr*)uexpr->operand)->value);

    printf("Expressions:\n");
    for (size_t i = 0; i < COUNTOF(exprs); i++) {
        expr_print(exprs[i], 2);
        printf("\n");
    }

    arena_reset(arena);
}

