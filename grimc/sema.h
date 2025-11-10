#ifndef GRIMC_SEMA_H
#define GRIMC_SEMA_H

#include "common.h"
#include "ast.h"

typedef struct Type Type;
typedef struct Sym Sym;
typedef struct Sym_Decl Sym_Decl;

typedef enum Sym_Kind {
    SYM_KIND_NONE = 0,
    SYM_KIND_VAR,
    SYM_KIND_CONST,
} Sym_Kind;

/// Represent a symbol (name) in the symbol table. Optionally, contains an index
/// if the symbol is part of an expression list. e.g. in:
///    `a, b, c := 1, 2, 3`
/// Every symbol has an associated Sym_Decl (symbol declaration). In the case of
/// expression lists, multiple symbols may share the same Sym_Decl.
/// E.g.: `a, b, c :: 3.14`
///
struct Sym {
    Sym_Kind    kind;
    String      name;
    u32         index;
    Sym_Decl*   decl;
};

typedef enum Sym_Decl_Kind {
    SYM_DECL_KIND_NONE = 0,
    SYM_DECL_KIND_OBJECT,
    SYM_DECL_KIND_TYPE,
    SYM_DECL_KIND_FUNC,
} Sym_Decl_Kind;

const char* sym_decl_kind_to_str(Sym_Decl_Kind kind);

typedef enum Sym_Decl_State {
    SYM_UNRESOLVED = 0,
    SYM_RESOLVING,
    SYM_RESOLVED,
} Sym_Decl_State;

/// A symbol declaration. See Sym for more details.
struct Sym_Decl {
    Sym_Decl_Kind   kind;
    Sym_Decl_State  state;
    Typespec*       typespec;
    Expr*           expr;
    Type*           type;
    union {
        i64         ival;
    };
};

typedef enum Type_Kind {
    TYPE_NONE = 0,
    TYPE_INCOMPLETE,
    TYPE_COMPLETING,
    TYPE_VOID,
    TYPE_INT,
    TYPE_FLT,
    TYPE_BOOL,
    TYPE_POINTER,
    TYPE_ARRAY,
    TYPE_STRUCT,
    TYPE_UNION,
    TYPE_ENUM,
    TYPE_FUNC,
} Type_Kind;

typedef struct Type_Field {
    const char* name;
    Type*       type;
    usize       offset;
    // @TODO: default value
} Type_Field;

struct Type {
    Type_Kind   kind;
    Sym*        symbol;
    usize       size;
    usize       alignment;

    union {
        struct {
            Type*   base;
        } ptr;
        struct {
            Type*   base;
            usize   length;
        } array;
        struct {
            Type_Field* fields;
            usize       field_count;
            usize       size;
            usize       align;
        } aggregate;
        struct {
            Type*       ret_type;
            Type**      param_types;
            usize       param_count;
        } func;
    };
};

typedef struct Cached_Array_Type {
    Type*   type;
    Type*   base;
    i64     length;
} Cached_Array_Type;

typedef struct Cached_Pointer_Type {
    Type*   type;
    Type*   base;
} Cached_Pointer_Type;

typedef struct Sym_Cache {
    Arena                   arena;
    Sym**                   syms;
    Sym**                   ordered_syms;
    Cached_Array_Type*      cached_array_types;
    Cached_Pointer_Type*    cached_pointer_types;
} Sym_Cache;

extern Type* INCOMPLETE_TYPE;
extern Type* COMPLETING_TYPE;
extern Type* VOID_TYPE;

void sym_install_decl_stmt(Sym_Cache *sc, Stmt* decl_stmt);
void sym_install_type(Sym_Cache* sc, String name, Type* type);
Sym* sym_get(Sym_Cache* sc, String name);
void sym_resolve(Sym_Cache* sc, Sym* sym);

typedef struct Resolve_Expr {
    Type*   type;
    bool    is_const;
    union {
        i64     ival;
        u64     uval;
        double  fval;
        bool    bval;
    };
} Resolved_Expr;

Resolved_Expr resolve_expr(Sym_Cache* sc, Expr* expr);
Resolved_Expr resolve_const_expr(Sym_Cache* sc, Expr* expr);

DECL_TEST(sema);

#endif // GRIM_AST_H
