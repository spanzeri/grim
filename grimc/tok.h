//
//
//

#ifndef GRIM_TOK_H
#define GRIM_TOK_H

#include "common.h"

typedef enum Keyword {
    KW_NONE = 0,
    KW_PROC,
    KW_RETURN,
    KW_IF,
    KW_ELSE,
    KW_WHILE,
    KW_FOR,
    KW_DO,
    KW_SWITCH,
    KW_BREAK,
    KW_CONTINUE,
    KW_CONST,
    KW_ENUM,
    KW_STRUCT,
    KW_UNION,
} Keyword;

typedef enum Token_Kind {
    TOK_EOF = 0,
    TOK_KEYWORD,
    FIRST_CHAR_TOKEN = 32,
    TOK_PLUS = '+',
    TOK_MINUS = '-',
    TOK_ASTERISK = '*',
    TOK_SLASH = '/',
    TOK_PERCENT = '%',
    TOK_CARET = '^',
    TOK_TILDE = '~',
    TOK_DOT = '.',
    TOK_COMMA = ',',
    TOK_SEMICOLON = ';',
    TOK_COLON = ':',
    TOK_QUESTION = '?',
    TOK_LPAREN = '(',
    TOK_RPAREN = ')',
    TOK_LBRACE = '{',
    TOK_RBRACE = '}',
    TOK_LBRACKET = '[',
    TOK_RBRACKET = ']',
    TOK_LT = '<',
    TOK_GT = '>',
    TOK_BANG = '!',
    TOK_EQUAL = '=',
    TOK_AT = '@',
    TOK_DOLLAR = '$',
    TOK_HASH = '#',
    TOK_PIPE = '|',
    TOK_AMP = '&',
    FIRST_NONCHAR_TOKEN = 128,
    TOK_INT_LITERAL,
    TOK_FLT_LITERAL,
    TOK_CHAR_LITERAL,
    TOK_STRING_LITERAL,
    TOK_IDENTIFIER,
    TOK_VAR_ASSIGN,        // :=
    TOK_CONST_ASSIGN,      // ::=
    TOK_EQ,
    TOK_NEQ,
    TOK_LTEQ,
    TOK_GTEQ,
    TOK_AND,
    TOK_OR,
    TOK_BIT_AND_ASSIGN,     // &=
    TOK_BIT_OR_ASSIGN,      // |=
    TOK_AND_ASSIGN,         // &&=
    TOK_OR_ASSIGN,          // ||=
    TOK_ADD_ASSIGN,         // +=
    TOK_SUB_ASSIGN,         // -=
    TOK_MUL_ASSIGN,         // *=
    TOK_DIV_ASSIGN,         // /=
    TOK_MOD_ASSIGN,         // %=
    TOK_XOR_ASSIGN,         // ^=
    TOK_INCREMENT,          // ++
    TOK_DECREMENT,          // --
    TOK_LSHIFT,
    TOK_RSHIFT,
    TOK_LSHIFT_ASSIGN,      // <<=
    TOK_RSHIFT_ASSIGN,      // >>=
    TOK_DEREF,              // .*
    TOK_NULLABLE_ACCESS,    // .?

    TOK_COUNT,
} Token_Kind;

typedef struct Token {
    Token_Kind  kind;
    const char* start;
    const char* end;
    union {
        u64         ivalue;
        double      fvalue;
        const char *name;
        u32         cvalue; // We don't yet support multi-byte, but make it large enough for UTF-32
        char *      svalue; // Array, needs to be freed
        Keyword     keyword;
    };
} Token;

// WARNING: this returns a pointer to a static. The next call will overwrite it.
const char* token_kind_to_string(Token_Kind kind);
const char* keyword_to_string   (Keyword kw);
void        token_print         (Token tok);

#endif // GRIM_TOK_H

