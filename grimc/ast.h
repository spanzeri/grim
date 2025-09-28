#ifndef GRIM_AST_H
#define GRIM_AST_H

#include "common.h"
#include "lex.h"

typedef struct Expr Expr;
typedef struct Decl Decl;
typedef struct Stmt Stmt;
typedef struct Typespec Typespec;

typedef enum Stmt_Kind {
    STMT_NONE = 0,
    STMT_ASSIGNMENT,
    STMT_EXPR,
    STMT_RETURN,
    STMT_BREAK,
    STMT_CONTINUE,
    STMT_BLOCK,
    STMT_IF,
    STMT_FOR,
    STMT_WHILE,
    STMT_DO,
    STMT_SWITCH,
} Stmt_Kind;

typedef struct Case_Branch {
    // @IMPROVE: Support for ranges.
    // @IMPROVE: Support for more complex case values (expressions)?
    Expr**  values;
    int     value_count;
    Stmt*   body;
} Case_Branch;

struct Stmt {
    Stmt_Kind kind;
    union {
        struct {
            Token_Kind  op;
            Expr*       left;
            Expr*       right;
        } assignment;

        Expr* expr;

        struct {
            String label;
        } control;

        struct {
            Stmt**  stmts;
            int     stmt_count;
        } block;

        struct {
            Expr*   condition;
            Stmt*   then_branch;
            Stmt*   else_branch;
        } if_stmt;

        struct {
            Expr*   condition;
            Stmt*   body;
        } while_stmt;

        struct {
            Case_Branch**   cases;
            int             case_count;
            Stmt*           default_case;
        } switch_stmt;
    };
};

Stmt* stmt_assignment   (Token_Kind op, Expr* left, Expr* right);
Stmt* stmt_expr         (Expr* expr);
Stmt* stmt_return       (Expr* expr);
Stmt* stmt_block        (Stmt** stmts, int stmt_count);
Stmt* stmt_break        (const char* label);
Stmt* stmt_continue     (const char* label);
Stmt* stmt_if           (Expr* condition, Stmt* then_branch, Stmt* else_branch);
Stmt* stmt_while        (Expr* condition, Stmt* body);
Stmt* stmt_do_while     (Stmt* body, Expr* condition);
Stmt* stmt_switch       (Case_Branch** cases, int case_count, Stmt* default_case);

typedef enum Timespec_Kind {
    TYPESPEC_NONE = 0,
    TYPESPEC_NAME,
    TYPESPEC_ARRAY,
    TYPESPEC_PROC,
    TYPESPEC_POINTER,
} Typespec_Kind;

struct Typespec {
    Typespec_Kind kind;
    union {
        const char*     name;

        struct {
            Typespec*   base;
            Expr*       size;
            bool        is_const;
        } array;

        struct {
            Typespec*   return_type;
            Typespec**  params;
            int         param_count;
        } proc;

        struct {
            Typespec*   base;
            bool        is_const;
        } pointer;
    };
};

Typespec* typespec_name(const char* name);
Typespec* typespec_array(Typespec* base, Expr* size, bool is_const);
Typespec* typespec_proc(Typespec** params, int param_count, Typespec* return_type);
Typespec* typespec_pointer(Typespec* base, bool is_const);

typedef enum Decl_Kind {
    DECL_NONE = 0,
    DECL_ENUM,
    DECL_STRUCT,
    DECL_UNION,
    DECL_PROC,
} Decl_Kind;

typedef struct Enum_Item {
    String  name;
    Expr*   value;
} Enum_Item;

typedef struct Aggregate_Item {
    String*     names;
    int         names_count;
    Typespec*   type;
    Expr*       default_value;
} Aggregate_Item;

typedef struct Proc_Decl {
    Aggregate_Item* args;
    int             arg_count;
    Typespec*       return_type;
    Stmt*           body;
} Proc_Decl;

struct Decl {
    Decl_Kind   kind;
    const char *name;
    union {
        struct {
            Enum_Item*  items;
            int         item_count;
            Proc_Decl*  methods;
            int         method_count;
        } enum_decl;

        struct {
            Aggregate_Item* items;
            int             item_count;
            Proc_Decl*      methods;
            int             method_count;
        } aggregate_decl;

        Proc_Decl proc_decl;
    };
};

Decl* decl_enum(const char* name, Enum_Item* items, int item_count, Proc_Decl* methods, int method_count);
Decl* decl_struct(const char* name, Aggregate_Item* items, int item_count, Proc_Decl* methods, int method_count);
Decl* decl_union(const char* name, Aggregate_Item* items, int item_count, Proc_Decl* methods, int method_count);
Decl* decl_proc(const char* name, Aggregate_Item* args, int arg_count, Typespec* return_type, Stmt* body);

typedef enum Expr_Kind {
    EXPR_NONE = 0,
    EXPR_LIST,
    EXPR_INT,
    EXPR_FLT,
    EXPR_STR,
    EXPR_NAME,
    EXPR_CALL,
    EXPR_UNARY,
    EXPR_BINARY,
    EXPR_TERNARY,
    EXPR_CAST,
    EXPR_INDEX,
    EXPR_COMPOUND,
    EXPR_DECL,
    EXPR_ASSIGNMENT,
} Expr_Kind;

typedef struct Expr_List {
    Expr**  exprs;
    int     expr_count;
} Expr_List;

typedef struct Compound_Initializer {
    enum {
        NAMED,
        ORDERED,
        INDEXED,
    } kind;
    struct {
        Stmt* named;
        Expr* ordered;

        struct {
            Expr* index;
            Stmt* assignment;
        } indexed;
    };
} Compound_Initializer;

struct Expr {
    Expr_Kind kind;
    union {
        Expr_List       list;

        u64             ivalue;
        double          fvalue;
        const char*     svalue;
        const char*     name;

        struct {
            const char* name;
            Expr**      args;
            int         arg_count;
        } call;

        struct {
            Token_Kind  op;
            Expr*       operand;
        } unary;

        struct {
            Token_Kind  op;
            Expr*       left;
            Expr*       right;
        } binary;

        struct {
            Expr* condition;
            Expr* then_expr;
            Expr* else_expr;
        } ternary;

        struct {
            Typespec*   type;
            Expr*       expr;
        } cast;

        struct {
            Expr*       expr;
            Expr*       index;
        } index;

        struct {
            Typespec*               type;
            Compound_Initializer*   initalizers;
            int                     initializer_count;
        } compound;

        struct {
            Token_Kind  op;
            Expr*       left;
            Expr*       right;
        } assignment;

        Decl*           decl;
    };
};


Expr* expr_list(Expr** exprs, int expr_count);
Expr* expr_int(u64 value);
Expr* expr_flt(double value);
Expr* expr_str(const char* str);
Expr* expr_name(const char* name);
Expr* expr_call(const char* name, Expr** arg_list, int arg_count);
Expr* expr_unary(Token_Kind op, Expr* operand);
Expr* expr_binary(Token_Kind op, Expr* left, Expr* right);
Expr* expr_ternary(Expr* condition, Expr* then_expr, Expr* else_expr);
Expr* expr_cast(Typespec* type, Expr* expr);
Expr* expr_index(Expr* expr, Expr* index);
Expr* expr_compound(Typespec* type, Compound_Initializer* initializers, int initializer_count);
Expr* expr_decl(Decl* decl);
Expr* expr_assignment(Token_Kind op, Expr* left, Expr* right);

// Print functions
//

void print_stmt(Stmt* stmt, int indent);
void print_typespec(Typespec* type, int indent);
void print_decl(Decl* decl, int indent);
void print_expr(Expr* expr, int indent);

DECL_TEST(ast);

#endif

