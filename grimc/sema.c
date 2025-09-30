#include "sema.h"

TEST(sema) {
    #if 0
    const char* foo = str_intern("foo");
    ASSERT(sym_get(foo) == NULL);
    Decl* decl = decl_const(foo, expr_int(42));
    sym_put(decl);
    Sym *sym = sym_get(foo);
    assert(sym && sym->decl == decl);

    Type *int_ptr = type_ptr(type_int);
    assert(type_ptr(type_int) == int_ptr);
    Type *float_ptr = type_ptr(type_float);
    assert(type_ptr(type_float) == float_ptr);
    assert(int_ptr != float_ptr);
    Type *int_ptr_ptr = type_ptr(type_ptr(type_int));
    assert(type_ptr(type_ptr(type_int)) == int_ptr_ptr);
    Type *float4_array = type_array(type_float, 4);
    assert(type_array(type_float, 4) == float4_array);
    Type *float3_array = type_array(type_float, 3);
    assert(type_array(type_float, 3) == float3_array);
    assert(float4_array != float3_array);
    Type *int_int_func = type_func(&type_int, 1, type_int);
    assert(type_func(&type_int, 1, type_int) == int_int_func);
    Type *int_func = type_func(NULL, 0, type_int);
    assert(int_int_func != int_func);
    assert(int_func == type_func(NULL, 0, type_int));
    #endif
}
