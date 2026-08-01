#ifndef GRIM_AST_H
#define GRIM_AST_H

#include "common.h"
#include "lex.h"

typedef struct Stmt Stmt;
typedef struct Expr Expr;

typedef enum Stmt_Kind {
    STMT_NONE = 0,
    STMT_ASSIGNMENT,
    STMT_DECL_VAR,
    STMT_DECL_CONST,
    STMT_EXPR,
    STMT_RETURN,
    STMT_CONTROL_BREAK,
    STMT_CONTROL_CONTINUE,
    STMT_IF,
    STMT_SWITCH,
    STMT_BLOCK,
    STMT_FOR,
    STMT_RANGE_FOR,
} Stmt_Kind;

struct Stmt {
    Stmt_Kind       kind;
};

typedef struct Assignment_Stmt {
    Stmt           base;
    Token_Kind     op;
    Expr*          left;
    Expr*          right;
} Assignment_Stmt;

typedef struct Decl_Stmt {
    Stmt            base;
    Expr*           left;
    Expr*           typespec;
    Expr*           right;
} Decl_Stmt;

typedef struct Expr_Stmt {
    Stmt            base;
    Expr*           expr;
} Expr_Stmt;

typedef struct Return_Stmt {
    Stmt            base;
    Expr*           expr;
} Return_Stmt;

typedef struct Control_Stmt {
    Stmt            base;
    String          label;
} Control_Stmt;

typedef struct If_Stmt {
    Stmt            base;
    Expr*           init;
    Expr*           condition;
    Stmt*           then_branch;
    Stmt*           else_branch;
} If_Stmt;

typedef struct Switch_Stmt {
    Stmt            base;
    struct Switch_Case* cases;
    int             case_count;
    Stmt*           default_case;
} Switch_Stmt;

typedef struct Block_Stmt {
    Stmt            base;
    Stmt**          stmts;
    int             stmt_count;
    String          label;
} Block_Stmt;

typedef struct For_Stmt {
    Stmt            base;
    Stmt*           init;
    Expr*           condition;
    Stmt*           post;
    Stmt*           body;
    String          label;
} For_Stmt;

typedef struct Range_For_Stmt {
    Stmt            base;
    Expr*           iterator;
    Expr*           iterable;
    Stmt*           body;
    String          label;
} Range_For_Stmt;

typedef struct Switch_Case {
    Expr*           condition;
    Stmt*           body;
} Switch_Case;

Stmt* stmt_assignment   (Arena* arena, Token_Kind op, Expr* left, Expr* right);
Stmt* stmt_decl_var     (Arena* arena, Expr* left, Expr* typespec, Expr* right);
Stmt* stmt_decl_const   (Arena* arena, Expr* left, Expr* typespec, Expr* right);
Stmt* stmt_expr         (Arena* arena, Expr* expr);
Stmt* stmt_return       (Arena* arena, Expr* expr);
Stmt* stmt_break        (Arena* arena, String label);
Stmt* stmt_continue     (Arena* arena, String label);
Stmt* stmt_switch       (Arena* arena, Switch_Case* cases, int case_count, Stmt* default_case);
Stmt* stmt_block        (Arena* arena, Stmt** stmts, int stmt_count, String label);
Stmt* stmt_if           (Arena* arena, Expr* condition, Stmt* then_branch, Stmt* else_branch);
Stmt* stmt_if_init      (Arena* arena, Expr* init, Expr* condition, Stmt* then_branch, Stmt* else_branch);
Stmt* stmt_for          (Arena* arena, Stmt* init, Expr* condition, Stmt* post, Stmt* body, String label);
Stmt* stmt_range_for    (Arena* arena, Expr* iterator, Expr* iterable, Stmt* body, String label);

void stmt_print(Stmt* stmt, int indent);

typedef enum Expr_Kind {
    EXPR_NONE = 0,
    EXPR_LIST,
    EXPR_INT,
    EXPR_FLOAT,
    EXPR_BOOL,
    EXPR_NULL,
    EXPR_STRING,
    EXPR_NAME,
    EXPR_CALL,
    EXPR_UNARY,
    EXPR_BINARY,
    EXPR_TERNARY,
    EXPR_CAST,
    EXPR_INDEX,
    EXPR_SIZEOF,
    EXPR_ALIGNOF,
    EXPR_TYPEOF,
    EXPR_POINTER_TYPE,
    EXPR_ARRAY_TYPE,
    EXPR_FUNCTION,
    EXPR_AGGREGATE_STRUCT,
    EXPR_AGGREGATE_UNION,
    EXPR_ENUM,
} Expr_Kind;

struct Expr {
    Expr_Kind kind;
};

typedef struct List_Expr {
    Expr        base;
    Expr**      exprs;
    int         expr_count;
} List_Expr;

typedef enum Int_Expr_Flags {
    EXPR_INT_FLAG_NONE        = 0,
    EXPR_INT_FLAG_CHAR        = 1 << 0,
} Int_Expr_Flags;

typedef struct Int_Expr {
    Expr        base;
    u32         flags;
    u64         value;
} Int_Expr;

typedef struct Float_Expr {
    Expr        base;
    double      value;
} Float_Expr;

typedef struct Bool_Expr {
    Expr        base;
    bool        value;
} Bool_Expr;

typedef struct Null_Expr {
    Expr        base;
} Null_Expr;

typedef struct String_Expr {
    Expr        base;
    String      value;
} String_Expr;

typedef struct Name_Expr {
    Expr        base;
    String      name;
} Name_Expr;

typedef struct Call_Expr {
    Expr        base;
    String      name;
    Expr**      args;
    int         arg_count;
    // @TODO: Support named arguments
} Call_Expr;

typedef struct Unary_Expr {
    Expr        base;
    Token_Kind  op;
    Expr*       operand;
} Unary_Expr;

typedef struct Binary_Expr {
    Expr        base;
    Token_Kind  op;
    Expr*       left;
    Expr*       right;
} Binary_Expr;

typedef struct Ternary_Expr {
    Expr        base;
    Expr*       condition;
    Expr*       then_expr;
    Expr*       else_expr;
} Ternary_Expr;

typedef struct Cast_Expr {
    Expr        base;
    Expr*       type_expr;
    Expr*       value_expr;
} Cast_Expr;

typedef struct Index_Expr {
    Expr        base;
    Expr*       expr;
    Expr*       index;
} Index_Expr;

/**
 * This is used for operators that operate either or types or expressions:
 * sizeof, alignof, typeof.
 */
typedef struct Type_Operator_Expr {
    Expr        base;
    Expr*       expr;
} Type_Operator_Expr;

typedef struct Pointer_Type_Expr {
    Expr        base;
    Expr*       pointed_type;
} Pointer_Type_Expr;

typedef struct Array_Type_Expr {
    Expr        base;
    Expr*       element_type;
    Expr*       size_expr;
} Array_Type_Expr;

typedef struct Function_Param {
    String      name;
    Expr*       type;
    Expr*       default_value;
} Function_Param;

typedef struct Function_Expr {
    Expr            base;
    Function_Param* params;
    int             param_count;
    // @TODO: Named returns? Multiple returns?
    Expr*           return_type;
    Stmt*           body;
} Function_Expr;


/** Either a struct or a union declaration */
typedef struct Aggregate_Type_Expr {
    Expr    base;
    Stmt**  members;
    int     member_count;
} Aggregate_Type_Expr;

typedef struct Enum_Item {
    String  name;
    Expr*   value;
} Enum_Item;

typedef struct Enum_Expr {
    Expr            base;
    Enum_Item*      items;
    int             item_count;
    Stmt**          members;
    int             member_count;
} Enum_Expr;

Expr* expr_list         (Arena* arena, Expr** exprs, int expr_count);
Expr* expr_int          (Arena* arena, u64 value);
Expr* expr_bool         (Arena* arena, bool value);
Expr* expr_null         (Arena* arena);
Expr* expr_float        (Arena* arena, double value);
Expr* expr_str          (Arena* arena, String value);
Expr* expr_name         (Arena* arena, String name);
Expr* expr_call         (Arena* arena, String name, Expr** args, int arg_count);
Expr* expr_unary        (Arena* arena, Token_Kind op, Expr* operand);
Expr* expr_binary       (Arena* arena, Token_Kind op, Expr* left, Expr* right);
Expr* expr_ternary      (Arena* arena, Expr* condition, Expr* then_expr, Expr* else_expr);
Expr* expr_cast         (Arena* arena, Expr* type_expr, Expr* value_expr);
Expr* expr_index        (Arena* arena, Expr* expr, Expr* index);
Expr* expr_sizeof       (Arena* arena, Expr* expr);
Expr* expr_alignof      (Arena* arena, Expr* expr);
Expr* expr_typeof       (Arena* arena, Expr* expr);
Expr* expr_pointer_type (Arena* arena, Expr* pointed_type);
Expr* expr_array_type   (Arena* arena, Expr* element_type, Expr* size_expr);
Expr* expr_function     (Arena* arena, Function_Param* params, int param_count, Expr* return_type, Stmt* body);
Expr* expr_struct       (Arena* arena, Stmt** members, int member_count);
Expr* expr_union        (Arena* arena, Stmt** members, int member_count);
Expr* expr_enum         (Arena* arena, Enum_Item* items, int item_count, Stmt** methods, int method_count);

void expr_print(Expr* expr, int indent);

DECL_TEST(ast);

#endif

