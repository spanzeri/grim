#include "sema.h"

//
//
//

Type* INCOMPLETE_TYPE = &(Type){ .kind = TYPE_INCOMPLETE };
Type* COMPLETING_TYPE = &(Type){ .kind = TYPE_COMPLETING };
Type* INT_TYPE        = &(Type){ .kind = TYPE_INT, .size = 8, .alignment = 8 };
Type* FLT_TYPE        = &(Type){ .kind = TYPE_FLT, .size = 8, .alignment = 8 };
Type* BOOL_TYPE       = &(Type){ .kind = TYPE_BOOL, .size = 1, .alignment = 1 };
Type* VOID_TYPE       = &(Type){ .kind = TYPE_VOID, .size = 1, .alignment = 1 };

static Type* type_new(Arena* arena, Type_Kind kind)
{
    Type* type = arena_alloc(arena, sizeof(Type));
    memset(type, 0, sizeof(Type));
    type->kind   = kind;
    return type;
}

static Sym* sym_new(Arena* arena, Sym_Kind kind, const char* name, Sym_Decl* decl)
{
    Sym* sym = arena_alloc(arena, sizeof(Sym));
    memset(sym, 0, sizeof(Sym));
    sym->kind  = kind;
    sym->name  = str_from_cstr(name);
    sym->index = 0;
    sym->decl  = decl;
    return sym;
}

static Sym* sym_new_at_index(Arena* arena, Sym_Kind kind, const char* name, Sym_Decl* decl, u32 index)
{
    Sym* sym = sym_new(arena, kind, name, decl);
    sym->index = index;
    return sym;
}

static Sym_Decl* sym_decl_new(Arena* arena, Sym_Decl_Kind kind, Expr* expr, Typespec* typespec)
{
    Sym_Decl* decl = arena_alloc(arena, sizeof(Sym_Decl));
    memset(decl, 0, sizeof(Sym_Decl));
    decl->kind     = kind;
    decl->state    = SYM_UNRESOLVED;
    decl->expr     = expr;
    decl->typespec = typespec;
    return decl;
}

static Sym_Decl* sym_decl(Sym_Cache* sc, Expr* expr, Typespec* typespec)
{
    ASSERT(expr != NULL || typespec != NULL);
    if (expr == NULL && typespec == NULL) {
        return NULL;
    }

    Sym_Decl_Kind kind = SYM_DECL_KIND_OBJECT;
    if (expr != NULL && expr->kind == EXPR_TYPE_DECL) {
        switch (expr->decl->kind) {
            case TYPE_DECL_ENUM:    kind = SYM_DECL_KIND_TYPE;  break;
            case TYPE_DECL_STRUCT:  kind = SYM_DECL_KIND_TYPE;  break;
            case TYPE_DECL_UNION:   kind = SYM_DECL_KIND_TYPE;  break;
            case TYPE_DECL_FUNC:    kind = SYM_DECL_KIND_FUNC;  break;
            default:
                ASSERT_ALWAYS("Unexpected type decl kind: %d", expr->decl->kind);
                return NULL;
        }
    }

    Sym_Decl* decl = sym_decl_new(&sc->arena, kind, expr, typespec);
    if (kind == SYM_DECL_KIND_TYPE) {
        decl->state = SYM_RESOLVED;
        decl->type = INCOMPLETE_TYPE;
    }
    return decl;
}

Sym* sym_get(Sym_Cache* sc, String name)
{
    for (Sym** s = sc->syms; s != darray_end(sc->syms); s++) {
        Sym* sym = *s;
        if (str_eq(sym->name, name)) {
            return sym;
        }
    }
    return NULL;
}

//
// Symbols
//

void sym_install_decl_stmt(Sym_Cache* sc, Stmt* decl_stmt)
{
    ASSERT(decl_stmt->kind == STMT_DECL_CONST || decl_stmt->kind == STMT_DECL_VAR);
    if (decl_stmt->kind != STMT_DECL_CONST && decl_stmt->kind != STMT_DECL_VAR)
        return;

    Sym_Kind kind = (decl_stmt->kind == STMT_DECL_CONST) ? SYM_KIND_CONST : SYM_KIND_VAR;

    ASSERT(decl_stmt->decl.left != NULL);
    ASSERT(decl_stmt->decl.right != NULL || decl_stmt->decl.type != NULL);
    if (decl_stmt->decl.left == NULL || (decl_stmt->decl.right == NULL && decl_stmt->decl.type == NULL))
        return;

    Expr* lhs    = decl_stmt->decl.left;
    Expr* rhs    = decl_stmt->decl.right;
    Typespec* ts = decl_stmt->decl.type;

    if (rhs == NULL || rhs->kind != EXPR_LIST) {
        Sym_Decl* decl = sym_decl(sc, rhs, ts);
        if (decl == NULL) { return; }

        if (lhs->kind == EXPR_NAME) {
            Sym* sym = sym_new(&sc->arena, kind, lhs->name, decl);
            if (sym) {
                darray_add(sc->syms, sym);
            }
        }
        else if (lhs->kind == EXPR_LIST) {
            ASSERT(lhs->list.expr_count > 0);
            for (int i = 0; i < lhs->list.expr_count; i++) {
                Expr* le = lhs->list.exprs[i];
                if (le->kind != EXPR_NAME) {
                    syntax_error("Expected a name in the declaration list.");
                    continue;
                }
                Sym* sym = sym_new_at_index(&sc->arena, kind, le->name, decl, (u32)i);
                if (sym) {
                    darray_add(sc->syms, sym);
                }
            }
        }
        else {
            syntax_error("Expected an identifier on the left hand side of a declaration.");
        }
    }
    else {
        // Right hand side is a list.
        ASSERT(rhs->kind == EXPR_LIST);
        ASSERT(rhs->list.expr_count > 0);

        if (lhs->kind != EXPR_LIST || lhs->list.expr_count == 0) {
            syntax_error("Mismatched number of identifiers for declaration.");
            return;
        }

        for (int i = 0; i < rhs->list.expr_count; i++) {
            Expr* le = lhs->list.exprs[i];
            Expr* re = rhs->list.exprs[i];
            if (le->kind != EXPR_NAME) {
                syntax_error("Expected an identifier in the declaration list.");
                continue;
            }
            Sym_Decl* decl = sym_decl(sc, re, ts);
            Sym* sym = sym_new_at_index(&sc->arena, kind, le->name, decl, (u32)i);
            if (decl) {
                darray_add(sc->syms, sym);
            }
        }
    }
}

static Type* resolve_typespec(Sym_Cache* sc, Typespec* typespec);

static Sym* resolve_name(Sym_Cache* sc, String name)
{
    Sym* sym = sym_get(sc, name);
    if (!sym) {
        syntax_error("Undefined symbol: %.*s", STR_FMT(name));
        return NULL;
    }
    sym_resolve(sc, sym);
    return sym;
}

static void type_complete_structure(Type* type, Type_Field* fields, usize field_count)
{
    ASSERT(type->kind == TYPE_INCOMPLETE);
    type->kind = TYPE_STRUCT;
    type->aggregate.fields = fields;
    type->aggregate.field_count = field_count;

    usize offset = 0;
    usize max_align = 1;
    for (usize i = 0; i < field_count; i++) {
        Type_Field* field = &fields[i];
        if (field->type->alignment > max_align) {
            max_align = field->type->alignment;
        }
        offset = (offset + field->type->alignment - 1) & ~(field->type->alignment - 1);
        field->offset = offset;
        offset += field->type->size;
    }

    type->size = (offset + max_align - 1) & ~(max_align - 1);
    type->alignment = max_align;
}

// @NOTE: This defines how internally the union are stores. We store the union
// tag a 32 bit unsigned integer. We store it after the field storage.
static void type_complete_union(Type* type, Type_Field* fields, usize field_count)
{
    ASSERT(type->kind == TYPE_INCOMPLETE);
    type->kind = TYPE_UNION;
    type->aggregate.fields = fields;

    usize max_size = 0;
    usize max_align = 1;
    for (usize i = 0; i < field_count; i++) {
        Type_Field* field = &fields[i];
        if (field->type->size > max_size) {
            max_size = field->type->size;
        }
        if (field->type->alignment > max_align) {
            max_align = field->type->alignment;
        }
    }

    usize size = (max_size + max_align - 1) & ~(max_align - 1);
    size += 4; // Space for the union tag.
    type->size = (size + max_align - 1) & ~(max_align - 1);
    usize alignment = (max_align > 4) ? max_align : 4;
    type->alignment = alignment;
    type->aggregate.field_count = field_count;

}

static void complete_type(Sym_Cache* sc, Type* type)
{
    if (!type) { return; }

    if (type->kind == TYPE_COMPLETING) {
        syntax_error("Cyclic dependency detected while completing type.");
        return;
    } else if (type->kind != TYPE_INCOMPLETE) {
        return;
    }

    type->kind = TYPE_COMPLETING;

    Sym_Decl* decl = type->symbol->decl;
    ASSERT(decl);
    ASSERT(decl->kind == SYM_DECL_KIND_TYPE);
    ASSERT(decl->expr->kind == EXPR_TYPE_DECL);
    ASSERT(decl->expr->decl->kind == TYPE_DECL_STRUCT || decl->expr->decl->kind == TYPE_DECL_UNION);
    bool is_struct = (decl->expr->decl->kind == TYPE_DECL_STRUCT);
    bool is_union  = (decl->expr->decl->kind == TYPE_DECL_UNION);
    Type_Field* fields = NULL;
    for (int i = 0; i < decl->expr->decl->aggregate_decl.item_count; i++) {
        Aggregate_Item item = decl->expr->decl->aggregate_decl.items[i];
        Type* field_type = resolve_typespec(sc, item.type);
        complete_type(sc, field_type);
        for (usize j = 0; j < (usize)item.names_count; j++) {
            darray_add(fields, (Type_Field){ item.names[j].data, field_type, 0 });
        }
    }
    if (is_struct) {
        type_complete_structure(type, fields, (usize)darray_len(fields));
    } else if (is_union) {
        type_complete_union(type, fields, (usize)darray_len(fields));
    } else {
        UNREACHABLE();
    }
    darray_add(sc->ordered_syms, type->symbol);
}

static Type* type_array(Sym_Cache* sc, Type* base, i64 length)
{
    if (!base) {
        // @TODO: Do we need an error type?
        return NULL;
    }

    for (Cached_Array_Type* it = sc->cached_array_types; it != darray_end(sc->cached_array_types); it++) {
        if (it->base == base && it->length == length) {
            return it->type;
        }
    }
    complete_type(sc, base);
    Type* type = type_new(&sc->arena, TYPE_ARRAY);
    type->array.base    = base;
    type->array.length  = (usize)length;
    type->size          = base->size * (usize)length;
    type->alignment     = base->alignment;
    darray_add(sc->cached_array_types, ((Cached_Array_Type){ .type = type, .base = base, .length = length }));

    return type;
}

static Type* type_pointer(Sym_Cache* sc, Type* base)
{
    if (!base) {
        // @TODO: do we need an error type?
        return NULL;
    }

    for (Cached_Pointer_Type* it = sc->cached_pointer_types; it != darray_end(sc->cached_pointer_types); it++) {
        if (it->base == base) {
            return it->type;
        }
    }
    complete_type(sc, base);
    Type* type = type_new(&sc->arena, TYPE_POINTER);
    type->ptr.base = base;
    type->size = 8;       // @TODO: Platform specific pointer size
    type->alignment = 8;  // @TODO: Platform specific pointer alignment
    darray_add(sc->cached_pointer_types, ((Cached_Pointer_Type){ .type = type, .base = base }));
    return type;
}

static Type* type_func(Sym_Cache* sc, Type** param_types, int param_count, Type* ret_type)
{
    Type* type = type_new(&sc->arena, TYPE_FUNC);
    type->func.param_types = arena_dup_array(&sc->arena, param_types, param_count);
    type->func.param_count = (usize)param_count;
    type->func.ret_type    = ret_type;
    type->size             = 8;       // @TODO: Platform specific function pointer size
    type->alignment        = 8;       // @TODO: Platform specific function pointer alignment
    return type;
}

static Resolved_Expr resolve_expr_name(Sym_Cache* sc, Expr* expr)
{
    ASSERT(expr->kind == EXPR_NAME);
    Sym* sym = resolve_name(sc, str_from_cstr(expr->name));
    if (sym->kind == SYM_KIND_VAR) {
        return (Resolved_Expr){ sym->decl->type };
    } else if (sym->kind == SYM_KIND_CONST) {
        return (Resolved_Expr){ sym->decl->type, true, .ival = sym->decl->ival };
    } else {
        syntax_error("Expected a variable or constant, but got a type or function: %s", expr->name);
        return (Resolved_Expr){0};
    }
}

static Resolved_Expr resolve_expr_unary(Sym_Cache* sc, Expr* expr)
{
    ASSERT(expr->kind == EXPR_UNARY);
    Resolved_Expr operand = resolve_expr(sc, expr->unary.operand);
    switch (expr->unary.op) {
        case '-':
            if (operand.type != INT_TYPE && operand.type != FLT_TYPE) {
                syntax_error("Unary '-' operator requires an integer or float operand.");
                return (Resolved_Expr){0};
            }
            if (operand.is_const) {
                if (operand.type == INT_TYPE) {
                    return (Resolved_Expr){ .type = INT_TYPE, .is_const = true, .ival = -operand.ival };
                } else if (operand.type == FLT_TYPE) {
                    return (Resolved_Expr){ .type = FLT_TYPE, .is_const = true, .fval = -operand.fval };
                }
                UNREACHABLE();
            } else {
                return (Resolved_Expr){ .type = operand.type, .is_const = false };
            }

        case '!':
            if (operand.type != BOOL_TYPE) {
                syntax_error("Unary '!' operator requires a boolean type.");
                return (Resolved_Expr){0};
            }
            if (operand.is_const) {
                return (Resolved_Expr){ .type = BOOL_TYPE, .is_const = true, .bval = !operand.bval };
            } else {
                return (Resolved_Expr){ .type = BOOL_TYPE, .is_const = false };
            }

        case TOK_DEREF:
            if (operand.type->kind != TYPE_POINTER) {
                syntax_error("Unary '*' operator requires a pointer operand.");
                return (Resolved_Expr){0};
            }
            return (Resolved_Expr){ .type = operand.type->ptr.base, .is_const = false };

        default:
            syntax_error("Unsupported unary operator: %s", token_kind_to_string(expr->unary.op));
            return (Resolved_Expr){0};
    }
}

static Type* expr_get_common_type(Resolved_Expr* left, Resolved_Expr* right)
{
    if (left->type == right->type) {
        return left->type;
    }

    if ((left->type == INT_TYPE && right->type == FLT_TYPE) ||
        (left->type == FLT_TYPE && right->type == INT_TYPE)) {
        return FLT_TYPE;
    }

    return NULL;
}

static Resolved_Expr resolve_expr_binary(Sym_Cache* sc, Expr* expr)
{
    ASSERT(expr->kind == EXPR_BINARY);
    Resolved_Expr left  = resolve_expr(sc, expr->binary.left);
    Resolved_Expr right = resolve_expr(sc, expr->binary.right);

    switch (expr->binary.op) {
        case '+':
        case '-':
        case '*':
        case '/': {
            Type* common_type = expr_get_common_type(&left, &right);
            if (common_type != INT_TYPE && common_type != FLT_TYPE) {
                syntax_error("Binary arithmetic operators require integer or float operands.");
                return (Resolved_Expr){0};
            }

            Resolved_Expr result = (Resolved_Expr){
                .type = common_type,
                .is_const = left.is_const && right.is_const,
            };

            if (common_type == INT_TYPE) {
                result.ival = (
                    expr->binary.op == '+' ? (left.ival + right.ival) :
                    expr->binary.op == '-' ? (left.ival - right.ival) :
                    expr->binary.op == '*' ? (left.ival * right.ival) :
                    expr->binary.op == '/' ? (left.ival / right.ival) : 0
                );
            } else {
                double val1 = (left.type == INT_TYPE) ? (double)left.ival : left.fval;
                double val2 = (right.type == INT_TYPE) ? (double)right.ival : right.fval;
                result.fval = (
                    expr->binary.op == '+' ? (val1 + val2) :
                    expr->binary.op == '-' ? (val1 - val2) :
                    expr->binary.op == '*' ? (val1 * val2) :
                    expr->binary.op == '/' ? (val1 / val2) : 0.0
                );
            }
            return result;
        }

        default:
            syntax_error("Unsupported binary operator: %s", token_kind_to_string(expr->binary.op));
            return (Resolved_Expr){0};
    }
}

Resolved_Expr resolve_expr(Sym_Cache* sc, Expr* expr)
{
    switch (expr->kind) {
        case EXPR_NAME:     return resolve_expr_name(sc, expr);
        case EXPR_UNARY:    return resolve_expr_unary(sc, expr);
        case EXPR_BINARY:   return resolve_expr_binary(sc, expr);
        case EXPR_INT:      return (Resolved_Expr){ .type = INT_TYPE,   .is_const = true, .ival = (i64)expr->ivalue }; // @TODO: Handle different integer sizes.
        case EXPR_FLT:      return (Resolved_Expr){ .type = FLT_TYPE,   .is_const = true, .fval = expr->fvalue };
        case EXPR_BOOL:     return (Resolved_Expr){ .type = BOOL_TYPE,  .is_const = true, .bval = expr->bvalue };
        case EXPR_SIZEOF_EXPR: {
            // @TODO: Handle names that could be types or variables.
            Resolved_Expr se = resolve_expr(sc, expr->sizeof_expr);
            complete_type(sc, se.type);
            return (Resolved_Expr){ .type = INT_TYPE, .is_const = true, .uval = se.type->size };
        }
        case EXPR_SIZEOF_TYPE: {
            Type* st = resolve_typespec(sc, expr->sizeof_type);
            complete_type(sc, st);
            return (Resolved_Expr){ .type = INT_TYPE, .is_const = true, .uval = st->size };
        }

        default:
            ASSERT_ALWAYS("Unimplemented expression kind: %d", expr->kind);
            return (Resolved_Expr){0};
    }
}

Resolved_Expr resolve_const_expr(Sym_Cache* sc, Expr* expr)
{
    Resolved_Expr re = resolve_expr(sc, expr);
    if (!re.is_const) {
        syntax_error("Expected a constant expression.");
    }
    return re;
}

static Type* resolve_typespec(Sym_Cache* sc, Typespec* typespec)
{
    switch (typespec->kind) {
        case TYPESPEC_NAME: {
            Sym* sym = resolve_name(sc, str_from_cstr(typespec->name));
            if (!sym) {
                return INCOMPLETE_TYPE;
            }
            if (sym->decl->kind != SYM_DECL_KIND_TYPE) {
                syntax_error("Expected a type name, but got a variable or constant name: %s", typespec->name);
                return INCOMPLETE_TYPE;
            }
            sym->decl->type->symbol = sym;
            return sym->decl->type;

        } break;
        case TYPESPEC_ARRAY: {
            // @TODO: Const-ness
            Resolved_Expr size_expr = resolve_const_expr(sc, typespec->array.size);
            if (size_expr.type != INT_TYPE) {
                syntax_error("Array size must be an integer constant.");
                return INCOMPLETE_TYPE;
            }
            if (size_expr.ival <= 0) {
                syntax_error("Array size must be a positive integer.");
                return INCOMPLETE_TYPE;
            }
            return type_array(sc, resolve_typespec(sc, typespec->array.base), size_expr.ival);
        }
        case TYPESPEC_POINTER:
            // @TODO: Const-ness
            return type_pointer(sc, resolve_typespec(sc, typespec->pointer.base));
        case TYPESPEC_FUNC: {
            Type** args = NULL;
            for (int i = 0; i < typespec->func.param_count; i++) {
                darray_add(args, resolve_typespec(sc, typespec->func.params[i]));
            }
            Type* return_type =
                typespec->func.return_type ?
                resolve_typespec(sc, typespec->func.return_type) :
                VOID_TYPE;
            return type_func(sc, args, darray_len(args), return_type);
        } break;

        default:
            ASSERT_ALWAYS("Unimplemented typespec kind: %d", typespec->kind);
            UNREACHABLE();
    }
}

static Type* resolve_decl_const(Sym_Cache* sc, Sym_Decl* decl, i64* val)
{
    Resolved_Expr result = resolve_const_expr(sc, decl->expr);
    *val = result.ival;
    return result.type;
}

static Type* resolve_decl_var(Sym_Cache* sc, Sym_Decl* decl)
{
    Type* type = NULL;
    if (decl->typespec) {
        type = resolve_typespec(sc, decl->typespec);
    }
    if (decl->expr) {
        Resolved_Expr result = resolve_expr(sc, decl->expr);
        if (type && result.type != type) {
            syntax_error("Mismatched type"); // @TODO: Print what type we expected and what we got.
        }
        type = result.type;
    }
    complete_type(sc, type);
    return type;
}

static Type* resolve_decl_struct(Sym_Cache* sc, Sym* sym)
{
    Sym_Decl* decl = sym->decl;
    ASSERT(decl->expr && decl->expr->kind == EXPR_COMPOUND);
    Type* type = type_new(&sc->arena, TYPE_INCOMPLETE);
    type->symbol = sym;
    decl->type = type;
    complete_type(sc, type);
    return type;
}

static Type* resolve_decl_union(Sym_Cache* sc, Sym* sym)
{
    Sym_Decl* decl = sym->decl;
    ASSERT(decl->expr && decl->expr->kind == EXPR_COMPOUND);
    Type* type = type_new(&sc->arena, TYPE_INCOMPLETE);
    type->symbol = sym;
    decl->type = type;
    complete_type(sc, type);
    return type;
}

static Type* resolve_decl_enum(Sym_Cache* sc, Sym* sym)
{
    NOT_IMPLEMENTED();
    return NULL;
}

static Type* resolve_decl_func(Sym_Cache* sc, Sym* sym)
{
    NOT_IMPLEMENTED();
    return NULL;
}

void sym_install_type(Sym_Cache* sc, String name, Type* type)
{
    // @TODO: This sucks. Maybe we should have a sym_decl_kind_type?
    Sym_Decl* decl = sym_decl_new(&sc->arena, SYM_DECL_KIND_TYPE, NULL, NULL);
    decl->state = SYM_RESOLVED;
    decl->type  = type;

    Sym* sym = sym_new(&sc->arena, SYM_KIND_CONST, name.data, decl);
    darray_add(sc->syms, sym);
}

void sym_resolve(Sym_Cache* sc, Sym* sym)
{
    Sym_Decl* decl = sym->decl;
    if (decl->state == SYM_RESOLVED) {
        return;
    }

    printf("Resolving symbol: %.*s\n", STR_FMT(sym->name));

    if (decl->state == SYM_RESOLVING) {
        syntax_error("Cyclic dependency detected while resolving symbol: %.*s", STR_FMT(sym->name));
        return;
    }

    ASSERT(decl->state == SYM_UNRESOLVED);
    decl->state = SYM_RESOLVING;

    if (sym->kind == SYM_KIND_VAR && decl->kind != SYM_DECL_KIND_OBJECT) {
        syntax_error("Type or functions must be declared as constants. Use '::' instead of ':='");
        return;
    }

    switch (decl->kind) {
        case SYM_DECL_KIND_OBJECT:
            if (sym->kind == SYM_KIND_CONST) {
                decl->type = resolve_decl_const(sc, decl, &decl->ival);
            } else {
                decl->type = resolve_decl_var(sc, decl);
            }
            break;
        case SYM_DECL_KIND_TYPE: {
            Expr* expr = sym->decl->expr;
            ASSERT(expr->kind == EXPR_TYPE_DECL);
            switch (expr->decl->kind) {
                case TYPE_DECL_STRUCT:
                    decl->type = resolve_decl_struct(sc, sym);
                    break;
                case TYPE_DECL_UNION:
                    decl->type = resolve_decl_union(sc, sym);
                    break;
                case TYPE_DECL_ENUM:
                    decl->type = resolve_decl_enum(sc, sym);
                    break;
                case TYPE_DECL_FUNC:
                    decl->type = resolve_decl_func(sc, sym);
                    break;
                default:
                    ASSERT_ALWAYS("Expected a struct, union, enum, or function type declaration.");
                    UNREACHABLE();
                    break;
            }
        } break;

        default:
            ASSERT_ALWAYS("Unimplemented symbol declaration kind: %s", sym_decl_kind_to_str(decl->kind));
            UNREACHABLE();
            break;
    }
    decl->state = SYM_RESOLVED;
    darray_add(sc->ordered_syms, sym);
}

//
// Utils
//
const char* sym_decl_kind_to_str(Sym_Decl_Kind kind)
{
    switch (kind) {
        case SYM_DECL_KIND_NONE:    return "NONE";
        case SYM_DECL_KIND_OBJECT:  return "OBJECT";
        case SYM_DECL_KIND_TYPE:    return "TYPE";
        case SYM_DECL_KIND_FUNC:    return "FUNC";
    }
}

//
// Tests
//

static void resolve_test(void) {
    printf("Sema Test: Resolve Declarations\n");
    Sym_Cache* sym_cache = &(Sym_Cache){0};

    sym_install_type(sym_cache, str_from_lit("int"), INT_TYPE);

    const char* decls =
        "n :: 1 + sizeof(T);\n"
        "p :*int;\n"
        "T :: struct { i: [3]i32; }\n"
    ;

    Parse_Context pctx = parse_init(decls);
    parse_begin(&pctx);
    int stmt_count = darray_len(sym_cache->syms);
    while (!parse_is_at_end(&pctx)) {
        Stmt* stmt = parse_stmt(&pctx);
        ASSERT(stmt != NULL, "Expected statement");
        ASSERT(stmt->kind == STMT_DECL_CONST || stmt->kind == STMT_DECL_VAR, "Expected declaration");
        // printf("\n");
        sym_install_decl_stmt(sym_cache, stmt);
        stmt_count++;
        ASSERT(stmt_count == darray_len(sym_cache->syms), "Expected one symbol per declaration statement");
    }

    for (Sym** s = sym_cache->syms; s != darray_end(sym_cache->syms); s++) {
        Sym* sym = *s;
        sym_resolve(sym_cache, sym);
    }

    for (Sym** s = sym_cache->syms; s != darray_end(sym_cache->syms); s++) {
        Sym* sym = *s;
        printf("Sym: %.*s (index: %u)\n", STR_FMT(sym->name), sym->index);
    }

    parse_end(&pctx);
    parse_shutdown(&pctx);

}

TEST(sema) {
    resolve_test();
}
