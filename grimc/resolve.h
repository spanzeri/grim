#ifndef GRIMC_RESOLVE_H
#define GRIMC_RESOLVE_H

#include "common.h"
#include "ast.h"

typedef struct Type Type;
typedef struct Symbol Symbol;

typedef enum Type_Kind {
    TYPE_NONE,
    TYPE_INCOMPLETE,
    TYPE_COMPLETING,
    TYPE_INT,
    TYPE_FLOAT,
    TYPE_POINTER,
    TYPE_ARRAY,
    TYPE_FUNCTION,
} Type_Kind;

struct Type {
    Type_Kind   kind;
    usize       size;
    union {
        struct {
            Type *element;
        } pointer;
        struct {
            Type *element;
            usize length;
        } array;
        struct {
            Type    **parameters;
            usize   parameter_count;
            Type    *return_type;
        } function;
    };
};

typedef enum Symbol_Kind {
    SYMBOL_NONE,
    SYMBOL_VARIABLE,
    SYMBOL_CONSTANT,
    SYMBOL_TYPE,
} Symbol_Kind;

typedef enum Symbol_State {
    SYMBOL_STATE_UNRESOLVED,
    SYMBOL_STATE_RESOLVING,
    SYMBOL_STATE_RESOLVED,
} Symbol_State;

struct Symbol {
    Symbol_Kind     kind;
    Symbol_State    state;
    String          name;
    Type           *type;
};

DECL_TEST(resolve);

#endif // GRIMC_RESOLVE_H
