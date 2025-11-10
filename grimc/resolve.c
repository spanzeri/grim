#include "resolve.h"
#include "parse.h"

#define POINTER_SIZE 8 // @TODO: platform dependent

Type *type_int   = &(Type){ .kind = TYPE_INT,   .size = 4 };
Type *type_float = &(Type){ .kind = TYPE_FLOAT, .size = 4 };

static Type* type_alloc(Type_Kind kind)
{
    Type* type = xcalloc(1, sizeof(Type));
    type->kind = kind;
    return type;
}

typedef struct Cached_Pointer_Type {
    Type    *element;
    Type    *pointer;
} Cached_Pointer_Type;

typedef struct Cached_Array_Type {
    Type    *element;
    usize   length;
    Type    *array;
} Cached_Array_Type;

typedef struct Cached_Function_Type {
    Type    **parameters;
    usize   parameter_count;
    Type    *return_type;
    Type    *function;
} Cached_Function_Type;

static Cached_Pointer_Type  *gs_cached_pointer_types    = NULL;
static Cached_Array_Type    *gs_cached_array_types      = NULL;
static Cached_Function_Type *gs_cached_function_types   = NULL;

static Type *type_pointer(Type *element)
{
    for (Cached_Pointer_Type *it = gs_cached_pointer_types; it != darray_end(gs_cached_pointer_types); it++) {
        if (it->element == element) {
            return it->pointer;
        }
    }
    Type *type = type_alloc(TYPE_POINTER);
    type->size = POINTER_SIZE;
    type->pointer.element = element;
    darray_add(gs_cached_pointer_types, ((Cached_Pointer_Type){ element, type }));
    return type;
}

static Type *type_array(Type *element, usize length)
{
    for (Cached_Array_Type *it = gs_cached_array_types; it != darray_end(gs_cached_array_types); it++) {
        if (it->element == element && it->length == length) {
            return it->array;
        }
    }
    Type *type = type_alloc(TYPE_ARRAY);
    type->size = element->size * length;
    type->array.element = element;
    type->array.length = length;
    darray_add(gs_cached_array_types, ((Cached_Array_Type){ element, length, type }));
    return type;
}

static Type *type_function(Type **parameters, usize parameter_count, Type *return_type)
{
    for (Cached_Function_Type *it = gs_cached_function_types; it != darray_end(gs_cached_function_types); it++) {
        if (it->parameter_count == parameter_count && it->return_type == return_type) {
            bool match = true;
            for (usize i = 0; i < parameter_count; i++) {
                if (it->parameters[i] != parameters[i]) {
                    match = false;
                    break;
                }
            }
            if (match) {
                return it->function;
            }
        }
    }

    Type *type = type_alloc(TYPE_FUNCTION);
    type->function.parameters      = memdup(parameters, sizeof(Type*) * parameter_count);
    type->function.parameter_count = parameter_count;
    type->function.return_type     = return_type;
    darray_add(gs_cached_function_types, ((Cached_Function_Type){
        .parameters      = type->function.parameters,
        .parameter_count = parameter_count,
        .return_type     = return_type,
        .function        = type
    }));
    return type;
}

static Symbol *symbol_alloc(Symbol_Kind kind, String name)
{
    Symbol* sym = xcalloc(1, sizeof(Symbol));
    sym->kind  = kind;
    sym->state = SYMBOL_STATE_UNRESOLVED;
    sym->name  = name;
    return sym;
}

static Symbol **gs_symbols = NULL;

static Symbol *symbol_install_type(String name, Type *type)
{
    Symbol *sym = symbol_alloc(SYMBOL_TYPE, name);
    sym->state = SYMBOL_STATE_RESOLVED;
    sym->type  = type;
    darray_add(gs_symbols, sym);
    return sym;
}

#if 0
static void resolve_test(void)
{
    const char* code =
        "n :: 1 + sizeof(p);\n"
        "p : *T;\n"
        "T :: struct { i: [n]i32; }\n";

    Parse_Context pctx = parse_init(code);
    parse_begin(&pctx);
    i32 count = 0;
    for (;;) {
        if (parse_is_at_end(&pctx)) { break; }
        Stmt* stmt = parse_stmt(&pctx);
        ASSERT(stmt != NULL, "Expected statement");
        resolve_register_decl(stmt);
        count++;
    }

    ASSERT(darray_len(g_sym_cache.symbols) == count,
           "Expected %d symbols, got %d", count, darray_len(g_sym_cache.symbols));

    resolve_all_symbols();

    parse_end(&pctx);
    parse_shutdown(&pctx);
}
#endif

TEST(resolve)
{
    Type *int_ptr = type_pointer(type_int);
    ASSERT(type_pointer(type_int) == int_ptr);
    Type *float_ptr = type_pointer(type_float);
    ASSERT(type_pointer(type_float) == float_ptr);
    ASSERT(int_ptr != float_ptr);
    Type *int_ptr_ptr = type_pointer(int_ptr);
    ASSERT(type_pointer(int_ptr) == int_ptr_ptr);
    ASSERT(type_pointer(float_ptr) != int_ptr_ptr);
    Type *int_array_4 = type_array(type_int, 4);
    ASSERT(type_array(type_int, 4) == int_array_4);
    Type *int_array_8 = type_array(type_int, 8);
    ASSERT(type_array(type_int, 8) == int_array_8);
    ASSERT(int_array_4 != int_array_8);
    Type *float_array_4 = type_array(type_float, 4);
    ASSERT(type_array(type_float, 4) == float_array_4);
    ASSERT(float_array_4 != int_array_4);
    Type *float_array_3 = type_array(type_float, 3);
    ASSERT(type_array(type_float, 3) == float_array_3);
    ASSERT(float_array_3 != float_array_4);
    Type *func_type_1 = type_function(&int_ptr, 1, type_int);
    ASSERT(type_function(&int_ptr, 1, type_int) == func_type_1);
    Type *func_type_2 = type_function(&float_ptr, 1, NULL);
    ASSERT(type_function(&float_ptr, 1, NULL) == func_type_2);
    ASSERT(func_type_1 != func_type_2);
    Type *func_type_3_params[] = { int_ptr, float_ptr };
    Type *func_type_3 = type_function(func_type_3_params, 2, type_float);
    ASSERT(type_function(func_type_3_params, 2, type_float) == func_type_3);
    ASSERT(func_type_3 != func_type_1);
    ASSERT(func_type_3 != func_type_2);

    Symbol* sym_int = symbol_install_type(str_from_lit("int"), type_int);
    ASSERT(sym_int->kind == SYMBOL_TYPE);
    ASSERT(sym_int->state == SYMBOL_STATE_RESOLVED);

    const char *code = {
        "n :: 1+sizeof(*p)"
        "p :*T"
        "T :: struct { i: [sizeof(p)]int; }"
    };

    Parser parser = parser_init(code, NULL);

    while (!parser_is_at_end(&parser)) {
        Stmt *stmt = parser_next_stmt(&parser);
        ASSERT(parser_is_at_end(&parser) || stmt != NULL, "Expected statement");
    }

    parser_shutdown(&parser);
}

