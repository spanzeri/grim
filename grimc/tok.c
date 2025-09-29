#include "tok.h"

const char* token_names[TOK_COUNT] = {
    [TOK_EOF]             = "EOF",
    [TOK_KEYWORD]         = "KEYWORD",

    ['(']                 = "(",
    [')']                 = ")",
    ['{']                 = "{",
    ['}']                 = "}",
    ['[']                 = "[",
    [']']                 = "]",
    [';']                 = ";",
    [',']                 = ",",
    ['.']                 = ".",
    ['+']                 = "+",
    ['-']                 = "-",
    ['*']                 = "*",
    ['/']                 = "/",
    ['%']                 = "%",
    ['^']                 = "^",
    ['&']                 = "&",
    ['|']                 = "|",
    ['!']                 = "!",
    ['~']                 = "~",
    ['<']                 = "<",
    ['>']                 = ">",
    ['=']                 = "=",
    [':']                 = ":",

    [TOK_INT_LITERAL]     = "INT_LITERAL",
    [TOK_FLT_LITERAL]     = "FLT_LITERAL",
    [TOK_CHAR_LITERAL]    = "CHAR_LITERAL",
    [TOK_STRING_LITERAL]  = "STRING_LITERAL",
    [TOK_IDENTIFIER]      = "IDENTIFIER",
    [TOK_VAR_ASSIGN]      = ":=",
    [TOK_CONST_ASSIGN]    = "::",
    [TOK_EQ]              = "==",
    [TOK_NEQ]             = "!=",
    [TOK_LTEQ]            = "<=",
    [TOK_GTEQ]            = ">=",
    [TOK_AND]             = "AND",
    [TOK_OR]              = "OR",
    [TOK_BIT_AND_ASSIGN]  = "&=",
    [TOK_BIT_OR_ASSIGN]   = "|=",
    [TOK_AND_ASSIGN]      = "&&=",
    [TOK_OR_ASSIGN]       = "||=",
    [TOK_ADD_ASSIGN]      = "+=",
    [TOK_SUB_ASSIGN]      = "-=",
    [TOK_MUL_ASSIGN]      = "*=",
    [TOK_DIV_ASSIGN]      = "/=",
    [TOK_MOD_ASSIGN]      = "%=",
    [TOK_XOR_ASSIGN]      = "^=",
    [TOK_INCREMENT]       = "++",
    [TOK_DECREMENT]       = "--",
    [TOK_LSHIFT]          = "<<",
    [TOK_RSHIFT]          = ">>",
    [TOK_LSHIFT_ASSIGN]   = "<<=",
    [TOK_RSHIFT_ASSIGN]   = ">>=",
    [TOK_DEREF]           = ".*",
    [TOK_NULLABLE_ACCESS] = ".?",
};

const char* token_kind_to_string(Token_Kind kind) {
    const char* name = token_names[kind];
    if (!name) {
        ASSERT_ALWAYS("Unknown token kind");
        return "<Unknown Token Kind>";
    }
    return name;
}

const char* keyword_to_string(Keyword kw) {
    switch (kw) {
        case KW_NONE:       ASSERT_ALWAYS("Invalid keyword"); return "NONE";
        case KW_PROC:       return "PROC";
        case KW_RETURN:     return "RETURN";
        case KW_IF:         return "IF";
        case KW_ELSE:       return "ELSE";
        case KW_WHILE:      return "WHILE";
        case KW_FOR:        return "FOR";
        case KW_DO:         return "DO";
        case KW_SWITCH:     return "SWITCH";
        case KW_BREAK:      return "BREAK";
        case KW_CONTINUE:   return "CONTINUE";
        case KW_CONST:      return "CONST";
        case KW_ENUM:       return "ENUM";
        case KW_STRUCT:     return "STRUCT";
        case KW_UNION:      return "UNION";
        case KW_CAST:       return "CAST";
        case KW_SIZEOF:     return "SIZEOF";
        case KW_ALIGNOF:    return "ALIGNOF";
        case KW_TRUE:       return "TRUE";
        case KW_FALSE:      return "FALSE";
        case KW_NULL:       return "NULL";
    }
}

void token_print(Token tok) {
    printf("Token{ kind=%s", token_kind_to_string(tok.kind));
    switch (tok.kind) {
        case TOK_KEYWORD:
            printf(", keyword=%s", keyword_to_string(tok.keyword));
            break;
        case TOK_INT_LITERAL:
            printf(", ivalue=%llu", tok.ivalue);
            break;
        case TOK_FLT_LITERAL:
            printf(", fvalue=%f", tok.fvalue);
            break;
        case TOK_CHAR_LITERAL:
            printf(", cvalue='%c' (%u)", (char)tok.cvalue, tok.cvalue);
            break;
        case TOK_STRING_LITERAL:
            printf(", svalue=\"%s\"", tok.svalue);
            break;
        case TOK_IDENTIFIER:
            printf(", text='%.*s'", (int)(tok.end - tok.start), tok.start);
            break;
        default:
            break;
    }
    printf(" }\n");
}

