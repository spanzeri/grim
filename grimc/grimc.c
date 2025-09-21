//
// Entry point for the grim-lang compiler
//

#include "core.h"

[[maybe_unused]]
static void test_darray(int argc, char *argv[]) {
    const char **arr = NULL;
    ASSERT(darray_len(arr) == 0, "Initial arr len = %td, expected 0", darray_len(arr));
    darray_add(arr, "Hello");
    darray_add(arr, "World");
    for (int i = 0; i < argc; i++) {
        darray_add(arr, argv[i]);
    }
    for (usize i = 0; i < darray_len(arr); i++) {
        printf("Array arr[%zu] = %s\n", i, arr[i]);
    }

    const char *extra[] = {
        "This", "is", "a", "test", "of", "dynamic", "arrays",
        "This", "is", "a", "test", "of", "dynamic", "arrays",
        "This", "is", "a", "test", "of", "dynamic", "arrays",
        "This", "is", "a", "test", "of", "dynamic", "arrays",
    };
    for (usize i = 0; i < COUNTOF(extra); i++) {
        darray_add(arr, extra[i]);
    }

    darray_free(arr);
    ASSERT(darray_len(arr) == 0, "After free arr len = %td, expected 0", darray_len(arr));
    ASSERT(arr == NULL, "After free arr = %p, expected NULL", (void*)arr);

    enum { ARR_COUNT = 1024 };
    int* int_arr = NULL;
    for (int i = 0; i < ARR_COUNT; i++) {
        darray_add(int_arr, i * 2);
    }
    ASSERT(darray_len(int_arr) == ARR_COUNT, "int_arr len = %td, expected %d", darray_len(int_arr), ARR_COUNT);
    for (int i = 0; i < ARR_COUNT; i++) {
        ASSERT(int_arr[i] == i * 2, "int_arr[%d] = %d, expected %d", i, int_arr[i], i * 2);
    }

    printf("Array length = %td, cap = %td\n", darray_len(arr), darray_cap(arr));

    darray_free(int_arr);
    ASSERT(darray_len(int_arr) == 0, "After free int_arr len = %td, expected 0", darray_len(int_arr));
    ASSERT(int_arr == NULL, "After free int_arr = %p, expected NULL", (void*)int_arr);
}

//
// String interning
//

typedef struct Intern_String {
    usize       len;
    const char *str;
} Intern_String;

static Intern_String *interns;

static const char *str_intern_range(const char *str, const char *end) {
    usize len = (usize)(end - str);
    for (Intern_String *it = interns; it < darray_end(interns); it++) {
        if (it->len == len && strncmp(it->str, str, len) == 0) {
            return it->str;
        }
    }

    char *new_str = xmalloc(len + 1);
    memcpy(new_str, str, len);
    new_str[len] = '\0';

    darray_add(interns, (Intern_String){ .len = len, .str = new_str });
    return new_str;
}

static const char *str_intern(const char *str) {
    return str_intern_range(str, str + strlen(str));
}

static void test_str_intern(void) {
    const char *s1 = str_intern("Hello, World!");
    const char *s2 = str_intern("Hello, World!");
    const char *s3 = str_intern("Goodbye, World!");
    ASSERT(s1 == s2);
    ASSERT(s1 != s3);
    const char hello[] = "Hello";
    const char *s4 = str_intern_range(hello, hello + COUNTOF(hello) - 1);
    const char *s5 = str_intern("Hello");
    ASSERT(s4 == s5);
    const char hello2[] = "Hello!";
    const char *s6 = str_intern_range(hello2, hello2 + COUNTOF(hello2) - 1);
    ASSERT(s4 != s6);
}

//
// Lexer
//

PRINTF_LIKE(1, 2)
static void syntax_error(const char *fmt, ...)  {
    fprintf(stderr, "Syntax error: ");
    va_list args;
    va_start(args, fmt);
    vfprintf(stderr, fmt, args);
    va_end(args);
    fputc('\n', stderr);
}

typedef enum Token_Kind {
    TOK_EOF = 0,
    FIRST_CHAR_TOKEN = 32,
    FIRST_NONCHAR_TOKEN = 128,
    TOK_INT_LITERAL,
    TOK_FLT_LITERAL,
    TOK_CHAR_LITERAL,
    TOK_STRING_LITERAL,
    TOK_IDENTIFIER,
    TOK_LET_ASSIGN,        // :=
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
    };
} Token;

// WARNING: this returns a pointer to a static. The next call will overwrite it.
static const char *token_kind_to_string(Token_Kind kind) {
    thread_local static char buffer[2];
    if (kind >= FIRST_CHAR_TOKEN && kind < FIRST_NONCHAR_TOKEN) {
        buffer[0] = (char)kind;
        buffer[1] = '\0';
        return buffer;
    }

    switch (kind) {
        case TOK_EOF:             return "EOF";
        case TOK_INT_LITERAL:     return "INT_LITERAL";
        case TOK_FLT_LITERAL:     return "FLT_LITERAL";
        case TOK_STRING_LITERAL:  return "STRING_LITERAL";
        case TOK_IDENTIFIER:      return "IDENTIFIER";
        case TOK_LET_ASSIGN:      return "LET_ASSIGN";
        case TOK_CONST_ASSIGN:    return "CONST_ASSIGN";
        case TOK_EQ:              return "EQ";
        case TOK_NEQ:             return "NEQ";
        case TOK_LTEQ:            return "LTEQ";
        case TOK_GTEQ:            return "GTEQ";
        case TOK_AND:             return "AND";
        case TOK_OR:              return "OR";
        case TOK_BIT_AND_ASSIGN:  return "BIT_AND_ASSIGN";
        case TOK_BIT_OR_ASSIGN:   return "BIT_OR_ASSIGN";
        case TOK_AND_ASSIGN:      return "AND_ASSIGN";
        case TOK_OR_ASSIGN:       return "OR_ASSIGN";
        case TOK_ADD_ASSIGN:      return "ADD_ASSIGN";
        case TOK_SUB_ASSIGN:      return "SUB_ASSIGN";
        case TOK_MUL_ASSIGN:      return "MUL_ASSIGN";
        case TOK_DIV_ASSIGN:      return "DIV_ASSIGN";
        case TOK_MOD_ASSIGN:      return "MOD_ASSIGN";
        case TOK_XOR_ASSIGN:      return "XOR_ASSIGN";
        case TOK_INCREMENT:       return "INCREMENT";
        case TOK_DECREMENT:       return "DECREMENT";
        case TOK_LSHIFT:          return "LSHIFT";
        case TOK_RSHIFT:          return "RSHIFT";
        case TOK_LSHIFT_ASSIGN:   return "LSHIFT_ASSIGN";
        case TOK_RSHIFT_ASSIGN:   return "RSHIFT_ASSIGN";
        case TOK_DEREF:           return "DEREF";
        case TOK_NULLABLE_ACCESS: return "NULLABLE_ACCESS";
        default:                 ASSERT_ALWAYS("TODO"); return "???";
    }
}

static void token_print(Token tok) {
    printf("Token{ kind=%s", token_kind_to_string(tok.kind));
    switch (tok.kind) {
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

typedef struct Lexer_Context {
    const char *source;
    const char *at;
} Lexer_Context;

static u32 lexer_peek_codepoint(Lexer_Context *l) {
    return (u32)(*l->at);
}

static u32 lexer_read_codepoint(Lexer_Context *l) {
    u32 c = lexer_peek_codepoint(l);
    if (c != '\0') { l->at++; }
    return c;
}

static bool lexer_match_codepoint(Lexer_Context *l, u32 expected) {
    if (lexer_peek_codepoint(l) == expected) {
        lexer_read_codepoint(l);
        return true;
    }
    return false;
}

static Lexer_Context lexer_init(const char *source) {
    return (Lexer_Context){
        .source     = source,
        .at         = source,
    };
}


static bool is_digit(u32 c) {
    return c >= '0' && c <= '9';
}

typedef enum Numeric_Parse_State {
    OK,
    INVALID,
    OVERFLOW,
} Numeric_Parse_State;

static u64 lexer_lex_integer(Lexer_Context *l, u32 base, Numeric_Parse_State* state) {
    // @TODO: In the case of large floating point numbers, the literal might not fit in u64.
    // We need to handle that case.

    u32 c = 0, p = 0;
    u64 value = 0;
    u64 overflow_threshold = U64_MAX / (u64)base;

    for (;;) {
        c = lexer_peek_codepoint(l);
        u32 digit = 0;
        if (base < 16 && (c == 'e' || c == 'E'))    { break; } // Floating point exponent
        else if (c >= 'a' && c <= 'f')              { digit = 10 + (c - 'a'); }
        else if (c >= 'A' && c <= 'F')              { digit = 10 + (c - 'A'); }
        else if (is_digit(c))                       { digit = (u32)(c - '0'); }
        else if (c == '_') {
            if (p == 0) {
                syntax_error("Numeric literal cannot start with underscore");
                *state = INVALID;
            }
            else if (p == '_') {
                syntax_error("Consecutive underscores in numeric literal");
                *state = INVALID;
            }
            p = c;
            lexer_read_codepoint(l);
            continue;
        }
        else{
            break;
        }

        lexer_read_codepoint(l);
        p = c;

        if (digit >= base) {
            syntax_error("Invalid digit '%c' for base %u", c, base);
            *state = INVALID;
        }

        if (*state == OK) {
            if (value < overflow_threshold || (value == overflow_threshold && digit == 0)) {
                value = value * (u64)base + (u64)digit;
            } else {
                *state = OVERFLOW;
            }
        }
    }

    if (p == '_') {
        syntax_error("Numeric literal cannot end with underscore");
        *state = INVALID;
    }

    return value;
}

static void lexer_scan_numeric_literal(Lexer_Context *l, Token *tok) {
    u32 base = 10;

    Numeric_Parse_State parse_state = OK;

    if (lexer_match_codepoint(l, '0')) {
        u32 c = lexer_peek_codepoint(l);
        if (c == '.' || c == 'e' || c == 'E' || c == 'p' || c == 'P') {
            // This is a floating-point literal starting with 0. We handle it below.
        } else if (!is_digit(c)) {
            if (c == 'x' || c == 'X') {
                base = 16;
            } else if (c == 'b' || c == 'B') {
                base = 2;
            } else if (c == 'o' || c == 'O') {
                base = 8;
            } else if (!is_digit(c)) {
                // @TODO: To figure out if this is a single 0 or an invalid sequence, we need to lookahead
                // to the next token. For now, we just treat it as a single 0.
                // It might be easier to special case this in the parser.
                tok->kind = TOK_INT_LITERAL;
                tok->ivalue = 0;
                return;
            }
            lexer_read_codepoint(l);
        }
    }

    u64 integer_part = lexer_lex_integer(l, base, &parse_state);
    u32 c = lexer_peek_codepoint(l);
    bool is_float = (base == 10 || base == 16) && c == '.';
    is_float |= (base == 10) && (c == 'e' || c == 'E');
    is_float |= (base == 16) && (c == 'p' || c == 'P');

    if (is_float) {
        // @TODO: We rely on the standard library here to parse a floating-point
        // number. We should roll out our own implementation at some point.

        if (c == '.') { lexer_read_codepoint(l); }
        const char* frac_start = l->at;
        lexer_lex_integer(l, base, &parse_state);
        if (base == 16 && l->at == frac_start) {
            syntax_error("Hexadecimal floating-point literal requires digits after the decimal point");
            parse_state = INVALID;
        }

        bool has_exponent = false;
        c = lexer_peek_codepoint(l);
        if (base == 16) {
            if (c == 'p' || c == 'P') {
                lexer_read_codepoint(l);
                has_exponent = true;
            } else {
                syntax_error("Hexadecimal floating-point literal requires a binary exponent (p or P)");
                parse_state = INVALID;
            }
        } else {
            if (c == 'e' || c == 'E') {
                lexer_read_codepoint(l);
                has_exponent = true;
            }
        }

        if (has_exponent) {
            c = lexer_peek_codepoint(l);
            if (c == '+' || c == '-') {
                lexer_read_codepoint(l);
            }
            u32 exp_start = (u32)(l->at - l->source);
            lexer_lex_integer(l, 10, &parse_state);
            if (l->at == l->source + exp_start) {
                syntax_error("Floating-point literal exponent requires digits");
                parse_state = INVALID;
            }
        }

        const char *start = tok->start;
        const char *end = l->at;
        char *float_end = NULL;
        tok->kind = TOK_FLT_LITERAL;
        tok->fvalue = strtod(start, (char**)&float_end);
        ASSERT(float_end == end, "Internal error: strtod and our parser disagreed on the floating-point literal");
        if (errno == ERANGE) {
            syntax_error("Floating-point literal overflow");
            tok->fvalue = DBL_MAX;
        } else if (float_end == start) {
            syntax_error("Invalid floating-point literal");
            tok->fvalue = 0.0;
        }
    } else {
        tok->kind = TOK_INT_LITERAL;
        switch (parse_state) {
            case OK:        tok->ivalue = integer_part; break;
            case INVALID:   tok->ivalue = 0;            break;
            case OVERFLOW:  tok->ivalue = U64_MAX;      break;
        }
    }
}

static void lexer_scan_char(Lexer_Context* l, Token* tok) {
    // @TODO: Should we support multi-byte characters and leave to the semantic
    // analysis to check if we are trying to store it somewhere it doesn't fit?
    tok->kind = TOK_CHAR_LITERAL;

    lexer_read_codepoint(l);
    u32 c = lexer_read_codepoint(l);
    if (c == '\'') {
        syntax_error("Character literal cannot be empty");
        tok->cvalue = 0;
        return;
    }

    if (c == '\\') {
        u32 esc = lexer_read_codepoint(l);
        switch (esc) {
            case '0':   tok->cvalue = '\0'; break;
            case 'a':   tok->cvalue = '\a'; break;
            case 'b':   tok->cvalue = '\b'; break;
            case 'f':   tok->cvalue = '\f'; break;
            case 'n':   tok->cvalue = '\n'; break;
            case 'r':   tok->cvalue = '\r'; break;
            case 't':   tok->cvalue = '\t'; break;
            case 'v':   tok->cvalue = '\v'; break;
            case '\\':  tok->cvalue = '\\'; break;
            case '\'':  tok->cvalue = '\''; break;
            case '\"':  tok->cvalue = '\"'; break;
            default:
                syntax_error("Unknown escape sequence: \\%c", (char)esc);
                tok->cvalue = 0;
                break;
        }
    } else {
        if (c == '\n' || c == '\r' || c == '\0') {
            syntax_error("Unterminated character literal");
            tok->cvalue = 0;
            return;
        }

        tok->cvalue = c;
    }

    if (!lexer_match_codepoint(l, '\'')) {
        syntax_error("Unterminated character literal");
    }
}

static void lexer_scan_string(Lexer_Context* l, Token* tok) {
    // @TODO: We are not currently parsing or storing the string buffer.
    // We will need to do that at some point, substituting escape sequences.

    tok->kind = TOK_STRING_LITERAL;
    lexer_read_codepoint(l); // Skip the opening quote

    char* buff = NULL;

    const char* str_start = l->at;
    tok->start = str_start;
    for (;;) {
        u32 c = lexer_read_codepoint(l);
        if (c == '\0' || c == '\n' || c == '\r') {
            syntax_error("Unterminated string literal");
            tok->end = l->at;
            break;
        } else if (c == '"') {
            break;
        } else if (c == '\\') {
            c = lexer_read_codepoint(l);
            if (c == '\0' || c == '\n' || c == '\r') {
                syntax_error("Unterminated string literal");
                tok->start = str_start;
                tok->end = l->at;
                return;
            }

            switch (c) {
                case 'a':   darray_add(buff, '\a'); break;
                case 'b':   darray_add(buff, '\b'); break;
                case 'f':   darray_add(buff, '\f'); break;
                case 'n':   darray_add(buff, '\n'); break;
                case 'r':   darray_add(buff, '\r'); break;
                case 't':   darray_add(buff, '\t'); break;
                case 'v':   darray_add(buff, '\v'); break;
                case '"':   darray_add(buff, '"');  break;
                case '\\':  darray_add(buff, '\\'); break;
                case '0':   darray_add(buff, '\0'); break;
                case 'x': case 'u': case 'U':
                    // @TODO:
                    ASSERT_ALWAYS("TODO: Hexadecimal escape sequences in string literals");
                break;
                default:
                    syntax_error("Unknown escape sequence: \\%c", (char)c);
                    break;
            }

        }
        else {
            darray_add(buff, (char)c);
        }
    }

    darray_add(buff, '\0'); // Null-terminate the string
    tok->end = l->at - 1;   // Exclude the closing quote
    tok->svalue = buff;
}

static Token lexer_next_token(Lexer_Context *l) {
    Token tok = {0};

repeat:
    tok.start = l->at;

#define CASE1(c, c1, k1) \
    case c: { \
        lexer_read_codepoint(l); \
        if (lexer_match_codepoint(l, c1)) { \
            tok.kind = k1; \
        } else { \
            tok.kind = (Token_Kind)c; \
        } \
    } break

#define CASE2(c, c1, k1, c2, k2) \
    case c: { \
        lexer_read_codepoint(l); \
        if (lexer_match_codepoint(l, c1)) { \
            tok.kind = k1; \
        } else if (lexer_match_codepoint(l, c2)) { \
            tok.kind = k2; \
        } else { \
            tok.kind = (Token_Kind)c; \
        } \
    } break

#define CASE2_ASSIGN(c, c1, k1, k1a, c2, k2) \
    case c: { \
        lexer_read_codepoint(l); \
        if (lexer_match_codepoint(l, c1)) { \
            if (lexer_match_codepoint(l, '=')) { \
                tok.kind = k1a; \
            } else { \
                tok.kind = k1; \
            } \
        } else if (lexer_match_codepoint(l, c2)) { \
            tok.kind = k2; \
        } else { \
            tok.kind = (Token_Kind)c; \
        } \
    } break

    u32 c = lexer_peek_codepoint(l);
    switch (c) {
        case ' ': case '\t': case '\n': case '\r':
            lexer_read_codepoint(l);
            goto repeat;

        case '\0': {
            lexer_read_codepoint(l);
            tok.kind = TOK_EOF;
        } break;

        // = ==
        CASE1('=', '=', TOK_EQ);
        // ! !=
        CASE1('!', '=', TOK_NEQ);

        // < <= << <<=
        CASE2_ASSIGN('<', '<', TOK_LSHIFT, TOK_LSHIFT_ASSIGN, '=', TOK_LTEQ);
        // > >= >> >>=
        CASE2_ASSIGN('>', '>', TOK_RSHIFT, TOK_RSHIFT_ASSIGN, '=', TOK_GTEQ);
        // & &= && &&=
        CASE2_ASSIGN('&', '&', TOK_AND, TOK_AND_ASSIGN, '=', TOK_BIT_AND_ASSIGN);
        // | |= || ||=
        CASE2_ASSIGN('|', '|', TOK_OR,  TOK_OR_ASSIGN,  '=', TOK_BIT_OR_ASSIGN);

        // + += ++
        CASE2('+', '=', TOK_ADD_ASSIGN, '+', TOK_INCREMENT);
        // - -= --
        CASE2('-', '=', TOK_SUB_ASSIGN, '-', TOK_DECREMENT);
        // * *=
        CASE1('*', '=', TOK_MUL_ASSIGN);
        // % %=
        CASE1('%', '=', TOK_MOD_ASSIGN);
        // ^ ^=
        CASE1('^', '=', TOK_XOR_ASSIGN);

        // : := ::
        CASE2(':', '=', TOK_LET_ASSIGN, ':', TOK_CONST_ASSIGN);

        // / /= /* and //
        case '/': {
            lexer_read_codepoint(l);
            if (lexer_match_codepoint(l, '=')) {
                tok.kind = TOK_DIV_ASSIGN;
            } else if (lexer_match_codepoint(l, '/')) {
                // Single-line comment
                while (lexer_peek_codepoint(l) != '\n' && lexer_peek_codepoint(l) != '\r' && lexer_peek_codepoint(l) != '\0') {
                    lexer_read_codepoint(l);
                }
                goto repeat;
            } else if (lexer_match_codepoint(l, '*')) {
                int depth = 1;
                for (;;) {
                    u32 c2 = lexer_read_codepoint(l);
                    if (c2 == '\0') {
                        syntax_error("Unterminated multi-line comment");
                        break;
                    } else if (c2 == '/' && lexer_match_codepoint(l, '*')) {
                        depth++;
                    } else if (c2 == '*' && lexer_match_codepoint(l, '/')) {
                        depth--;
                        if (depth == 0) break;
                    }
                }
                goto repeat;
            } else {
                tok.kind = (Token_Kind)'/';
            }
        } break;

        case '(':
        case ')': {
            lexer_read_codepoint(l);
            tok.kind = (Token_Kind)c;
        } break;

        case '\'': {
            lexer_scan_char(l, &tok);
        } break;

        case '"': {
            lexer_scan_string(l, &tok);
        } break;

        case '.': {
            lexer_read_codepoint(l);
            if (is_digit(lexer_peek_codepoint(l))) {
                l->at--; // Put back the '.'
                lexer_scan_numeric_literal(l, &tok);
            } else if (lexer_match_codepoint(l, '.')) {
                // @TODO: Ranges
                ASSERT_ALWAYS("TODO: Range operator ..");
            } else if (lexer_match_codepoint(l, '*')) {
                tok.kind = TOK_DEREF;
            } else if (lexer_match_codepoint(l, '?')) {
                tok.kind = TOK_NULLABLE_ACCESS;
            } else {
                tok.kind = (Token_Kind)'.';
            }
        } break;

        case '0': case '1': case '2': case '3': case '4':
        case '5': case '6': case '7': case '8': case '9': {
            lexer_scan_numeric_literal(l, &tok);
        } break;

        case 'a': case 'b': case 'c': case 'd': case 'e': case 'f': case 'g': case 'h': case 'i': case 'j':
        case 'k': case 'l': case 'm': case 'n': case 'o': case 'p': case 'q': case 'r': case 's': case 't':
        case 'u': case 'v': case 'w': case 'x': case 'y': case 'z':
        case 'A': case 'B': case 'C': case 'D': case 'E': case 'F': case 'G': case 'H': case 'I': case 'J':
        case 'K': case 'L': case 'M': case 'N': case 'O': case 'P': case 'Q': case 'R': case 'S': case 'T':
        case 'U': case 'V': case 'W': case 'X': case 'Y': case 'Z':
        case '_': {
            for (;;) {
                u32 c2 = lexer_peek_codepoint(l);
                if (!((c2 >= 'a' && c2 <= 'z') || (c2 >= 'A' && c2 <= 'Z') || is_digit(c2) || c2 == '_')) {
                    break;
                }
                lexer_read_codepoint(l);
            }
            tok.kind = TOK_IDENTIFIER;
            tok.name = str_intern_range(tok.start, l->at);
        } break;

        default:
            fprintf(stderr, "Unexpected character: '%c'\n", c);
            return (Token){ .kind = TOK_EOF };
    }

    tok.end = l->at;
    return tok;

#undef CASE1
#undef CASE2
#undef CASE2_ASSIGN
}

static void test_lex(void) {
    // Generic test
    //
    const char *source = "+XY=(XY)1234+994 _abC + _01_ + AZ09";
    Token expected_tokens[] = {
        { .kind = '+' },
        { .kind = TOK_IDENTIFIER, .start = "XY" },
        { .kind = '=' },
        { .kind = '(' },
        { .kind = TOK_IDENTIFIER, .start = "XY" },
        { .kind = ')' },
        { .kind = TOK_INT_LITERAL, .ivalue = 1234 },
        { .kind = '+' },
        { .kind = TOK_INT_LITERAL, .ivalue = 994 },
        { .kind = TOK_IDENTIFIER, .start = "_abC" },
        { .kind = '+' },
        { .kind = TOK_IDENTIFIER, .start = "_01_" },
        { .kind = '+' },
        { .kind = TOK_IDENTIFIER, .start = "AZ09" },
        { .kind = TOK_EOF },
    };

    Token* tokens = NULL;
    Lexer_Context lex = lexer_init(source);
    for (;;) {
        Token tok = lexer_next_token(&lex);
        darray_add(tokens, tok);
        if (tok.kind == TOK_EOF) break;
    }

    ASSERT(darray_len(tokens) == COUNTOF(expected_tokens),
        "Got %td tokens, expected %zu", darray_len(tokens), COUNTOF(expected_tokens));
    for (usize i = 0; i < darray_len(tokens); i++) {
        ASSERT(tokens[i].kind == expected_tokens[i].kind,
            "Token %zu: got kind %d, expected %d", i, tokens[i].kind, expected_tokens[i].kind);
        if (tokens[i].kind == TOK_INT_LITERAL) {
            ASSERT(tokens[i].ivalue == expected_tokens[i].ivalue,
                "Token %zu: got int_value %llu, expected %llu", i,
                tokens[i].ivalue, expected_tokens[i].ivalue);
        }
        else if (tokens[i].kind == TOK_IDENTIFIER) {
            ASSERT(
                tokens[i].name == str_intern(expected_tokens[i].start),
                "Token %zu: got identifier '%s', expected '%s'", i,
                tokens[i].name, expected_tokens[i].start);
        }
    }

    ASSERT(tokens[1].name == tokens[4].name,
        "Identifier interning failed: tokens[1] = '%s', tokens[4] = '%s'",
        tokens[1].name, tokens[4].name);
    ASSERT(strcmp(tokens[1].name, "XY") == 0,
        "Identifier interning failed: tokens[1] = '%s', expected 'XY'",
        tokens[1].name);

    for (usize i = 0; i < darray_len(tokens); i++) {
        token_print(tokens[i]);
    }

    darray_free(tokens);

#define TEST_INT_LITERAL(expected)                                                                              \
    tok = lexer_next_token(&lex);                                                                               \
    ASSERT(tok.kind == TOK_INT_LITERAL, "Expected INT_LITERAL, got %s", token_kind_to_string(tok.kind));        \
    ASSERT(tok.ivalue == (expected), "Expected ivalue %llu, got %llu", (u64)(expected), tok.ivalue)

#define TEST_FLT_LITERAL(expected)                                                                              \
    tok = lexer_next_token(&lex);                                                                               \
    ASSERT(tok.kind == TOK_FLT_LITERAL, "Expected FLT_LITERAL, got %s", token_kind_to_string(tok.kind));        \
    ASSERT(fabs(tok.fvalue - (expected)) < DBL_EPSILON, "Expected fvalue %f, got %f", expected, tok.fvalue)

#define TEST_CHAR_LITERAL(expected)                                                                             \
    tok = lexer_next_token(&lex);                                                                               \
    ASSERT(tok.kind == TOK_CHAR_LITERAL, "Expected CHAR_LITERAL, got %s", token_kind_to_string(tok.kind));      \
    ASSERT(                                                                                                     \
        tok.cvalue == (expected),                                                                               \
        "Expected cvalue '%c' (%u), got '%c' (%u)",                                                             \
        (char)(expected), (u32)(expected), (char)tok.cvalue, tok.cvalue)

#define TEST_STRING_LITERAL(expected)                                                                           \
    tok = lexer_next_token(&lex);                                                                               \
    ASSERT(tok.kind == TOK_STRING_LITERAL, "Expected STRING_LITERAL, got %s", token_kind_to_string(tok.kind));  \
    ASSERT(                                                                                                     \
        strncmp(tok.svalue, (expected), darray_len(tok.svalue) - 1) == 0                                        \
            && strlen(expected) == (darray_len(tok.svalue) - 1),                                                \
        "Expected svalue \"%s\", got \"%s\". Expected len: %llu, got: %llu.",                                   \
        (expected), tok.svalue, (u64)strlen(expected), (u64)(darray_len(tok.svalue) - 1));                      \
    darray_free(tok.svalue)

#define TEST_TOK_KIND(expected)                                                                 \
    tok = lexer_next_token(&lex);                                                               \
    ASSERT(tok.kind == (expected), "Expected token kind %s, got %s", token_kind_to_string(expected), token_kind_to_string(tok.kind))

    Token tok;

    // Integer test
    //
    lex = lexer_init(
        "0 1 42 1234567890 0x0 0x1 0xA 0xF 0x10 0x2A 0b0 0b1 0b10 0b101 0o0 0o7 0o10 0o9 1AC0 "
        "18446744073709551615 0xFFFFFFFFFFFFFFFF 18446744073709551616 0x_01 0x1__2 0x12_ 1_000_000 0b1010_1011");

    TEST_INT_LITERAL(0);
    TEST_INT_LITERAL(1);
    TEST_INT_LITERAL(42);
    TEST_INT_LITERAL(1234567890);
    TEST_INT_LITERAL(0);
    TEST_INT_LITERAL(1);
    TEST_INT_LITERAL(10);
    TEST_INT_LITERAL(15);
    TEST_INT_LITERAL(16);
    TEST_INT_LITERAL(42);
    TEST_INT_LITERAL(0);
    TEST_INT_LITERAL(1);
    TEST_INT_LITERAL(2);
    TEST_INT_LITERAL(5);
    TEST_INT_LITERAL(0);
    TEST_INT_LITERAL(7);
    TEST_INT_LITERAL(8);
    TEST_INT_LITERAL(0);        // Invalid digit '9' for base 8
    TEST_INT_LITERAL(0);        // Invalid digit 'A' for base 10
    TEST_INT_LITERAL(U64_MAX);
    TEST_INT_LITERAL(U64_MAX);
    TEST_INT_LITERAL(U64_MAX);  // Overflow
    TEST_INT_LITERAL(0);        // Numeric literal cannot start with underscore
    TEST_INT_LITERAL(0);        // Consecutive underscores in numeric literal
    TEST_INT_LITERAL(0);        // Numeric literal cannot end with underscore
    TEST_INT_LITERAL(1000000);
    TEST_INT_LITERAL(0b10101011);
    ASSERT(lexer_next_token(&lex).kind == TOK_EOF);

    // Floating-point test
    //
    lex = lexer_init("0.0 1.0 .150 .1e-4 3.14159 1e3 1E3 1.5e2 1.5E2 1.5e-2 1.5E-2 0x1.4p+3");

    TEST_FLT_LITERAL(0.0);
    TEST_FLT_LITERAL(1.0);
    TEST_FLT_LITERAL(0.15);
    TEST_FLT_LITERAL(0.00001);
    TEST_FLT_LITERAL(3.14159);
    TEST_FLT_LITERAL(1000.0);
    TEST_FLT_LITERAL(1000.0);
    TEST_FLT_LITERAL(150.0);
    TEST_FLT_LITERAL(150.0);
    TEST_FLT_LITERAL( 0.015);
    TEST_FLT_LITERAL(0.015);
    TEST_FLT_LITERAL(10.0);     // Hexadecimal floating-point literal
    ASSERT(lexer_next_token(&lex).kind == TOK_EOF);

    // Character literal test
    //
    lex = lexer_init("'a' 'Z' '0' '\\n' '\\t' '\\'' '\\\\' '\\0' '\\x'");

    TEST_CHAR_LITERAL('a');
    TEST_CHAR_LITERAL('Z');
    TEST_CHAR_LITERAL('0');
    TEST_CHAR_LITERAL('\n');
    TEST_CHAR_LITERAL('\t');
    TEST_CHAR_LITERAL('\'');
    TEST_CHAR_LITERAL('\\');
    TEST_CHAR_LITERAL('\0');
    TEST_CHAR_LITERAL(0);       // Unknown escape sequence: \x
    ASSERT(lexer_next_token(&lex).kind == TOK_EOF);

    // String literal test
    //
    lex = lexer_init("\"Hello, World!\" \"Line1\\nLine2\" \"Tab\\tCharacter\" \"Quote:\\\"\" \"Backslash:\\\\\" \"Unterminated string");

    TEST_STRING_LITERAL("Hello, World!");
    TEST_STRING_LITERAL("Line1\nLine2");
    TEST_STRING_LITERAL("Tab\tCharacter");
    TEST_STRING_LITERAL("Quote:\"");
    TEST_STRING_LITERAL("Backslash:\\");
    TEST_STRING_LITERAL("Unterminated string"); // Unterminated string literal
    ASSERT(lexer_next_token(&lex).kind == TOK_EOF);

    // Operators
    //
    lex = lexer_init("= == ! != < <= << <<= > >= >> >>= & && &&= | || ||= + += ++ - -= -- * *= / /= % %= ^ ^= := ::");
    TEST_TOK_KIND('=');
    TEST_TOK_KIND(TOK_EQ);
    TEST_TOK_KIND('!');
    TEST_TOK_KIND(TOK_NEQ);
    TEST_TOK_KIND('<');
    TEST_TOK_KIND(TOK_LTEQ);
    TEST_TOK_KIND(TOK_LSHIFT);
    TEST_TOK_KIND(TOK_LSHIFT_ASSIGN);
    TEST_TOK_KIND('>');
    TEST_TOK_KIND(TOK_GTEQ);
    TEST_TOK_KIND(TOK_RSHIFT);
    TEST_TOK_KIND(TOK_RSHIFT_ASSIGN);
    TEST_TOK_KIND('&');
    TEST_TOK_KIND(TOK_AND);
    TEST_TOK_KIND(TOK_AND_ASSIGN);
    TEST_TOK_KIND('|');
    TEST_TOK_KIND(TOK_OR);
    TEST_TOK_KIND(TOK_OR_ASSIGN);
    TEST_TOK_KIND('+');
    TEST_TOK_KIND(TOK_ADD_ASSIGN);
    TEST_TOK_KIND(TOK_INCREMENT);
    TEST_TOK_KIND('-');
    TEST_TOK_KIND(TOK_SUB_ASSIGN);
    TEST_TOK_KIND(TOK_DECREMENT);
    TEST_TOK_KIND('*');
    TEST_TOK_KIND(TOK_MUL_ASSIGN);
    TEST_TOK_KIND('/');
    TEST_TOK_KIND(TOK_DIV_ASSIGN);
    TEST_TOK_KIND('%');
    TEST_TOK_KIND(TOK_MOD_ASSIGN);
    TEST_TOK_KIND('^');
    TEST_TOK_KIND(TOK_XOR_ASSIGN);
    TEST_TOK_KIND(TOK_LET_ASSIGN);
    TEST_TOK_KIND(TOK_CONST_ASSIGN);
    TEST_TOK_KIND(TOK_EOF);

#undef TEST_TOK_KIND
#undef TEST_STRING_LITERAL
#undef TEST_CHAR_LITERAL
#undef TEST_FLT_LITERAL
#undef TEST_INT_LITERAL
}


typedef struct Parser_Context {
    Lexer_Context   lexer;
    Token           current;
} Parser_Context;

static void next_token(Parser_Context *p) {
    p->current = lexer_next_token(&p->lexer);
}

static inline bool is_token(Parser_Context* p, Token_Kind kind) {
    return p->current.kind == kind;
}

inline bool is_token_name(Parser_Context* p, const char* name) {
    return p->current.kind == TOK_IDENTIFIER && p->current.name == name;
}


static Parser_Context parser_init(const char *source) {
    Parser_Context p = {
        .lexer     = lexer_init(source),
    };
    next_token(&p);
    return p;
}

static bool match_token(Parser_Context *p, Token_Kind kind) {
    if (p->current.kind == kind) {
        next_token(p);
        return true;
    }
    return false;
}

static void expect_token(Parser_Context *p, Token_Kind kind) {
    if (p->current.kind != kind) {
        fatal("Expected token kind %d, got %d", kind, p->current.kind);
    }
    next_token(p);
}

/*

    expr = expr0
    expr0 = expr1 (['+' | '-') expr1)*
    expr1 = expr2 (['*' | '/') expr2)*
    expr2 = '-' expr2 | expr3
    expr3 = INT_LITERAL | '(' expr0 ')'

*/

static i32 parse_expr0(Parser_Context *p);

static i32 parse_expr3(Parser_Context *p) {
    if (match_token(p, '(')) {
        i32 value = parse_expr0(p);
        expect_token(p, ')');
        return value;
    } else if (is_token(p, TOK_INT_LITERAL)) {
        i32 value = (i32)p->current.ivalue;
        next_token(p);
        return value;
    } else {
        fatal("Expected '(' or INT_LITERAL, got %d", p->current.kind);
        return 0;
    }
}

static i32 parse_expr2(Parser_Context *p) {
    if (match_token(p, '-')) {
        return -parse_expr2(p);
    }
    return parse_expr3(p);
}

static i32 parse_expr1(Parser_Context *p) {
    i32 left = parse_expr2(p);
    for (;;) {
        if (match_token(p, '*')) {
            i32 right = parse_expr2(p);
            left *= right;
        } else if (match_token(p, '/')) {
            i32 right = parse_expr2(p);
            if (right == 0) { fatal("Division by zero"); }
            left /= right;
        } else {
            break;
        }
    }
    return left;
}

static i32 parse_expr0(Parser_Context *p) {
    i32 left = parse_expr1(p);
    for (;;) {
        if (match_token(p, '+')) {
            i32 right = parse_expr1(p);
            left += right;
        } else if (match_token(p, '-')) {
            i32 right = parse_expr1(p);
            left -= right;
        } else {
            break;
        }
    }
    // @TODO: This is a temporary sanity check while we only parse expressions
    return left;
}

static i32 parse_expr_str(const char *source) {
    Parser_Context p = parser_init(source);
    i32 res = parse_expr0(&p);
    ASSERT(match_token(&p, TOK_EOF), "Expected EOF, got %d", p.current.kind);
    return res;
}

#define TEST_EXPR(expr) \
    do { \
        i32 __parse_res = parse_expr_str(#expr); \
        ASSERT(__parse_res == (expr),  \
            "Expression '%s' evaluated to %d, expected %d", #expr, __parse_res, (expr)); \
        fprintf(stdout, "Parsed expression '%s'.\n  > Evaluation result: %d\n", #expr, __parse_res); \
    } while (0)

static void test_parser(void) {
    TEST_EXPR((2));
    TEST_EXPR(1 + 2);
    TEST_EXPR(1 + 2 * 3 + 5);
    TEST_EXPR(-1);
    TEST_EXPR(1 + 2 * 3 - 4 / 2 + (5 - 6) * 7);
    TEST_EXPR(42);
    TEST_EXPR((1 + 2) * (3 + 5));
    TEST_EXPR((-1000));
    TEST_EXPR((- -69));
}

#undef TEST_EXPR

//
// Bytecode VM
//

typedef enum Op_Code {
    OP_HALT = 0,
    OP_ADD,
    OP_SUB,
    OP_MUL,
    OP_DIV,
    OP_NEG,
    OP_LIT,
} Op_Code;


static i32 vm_exec(u8* code) {
    enum { MAX_STACK = 1024 };

    i32 stack[MAX_STACK];
    i32 *top = stack;

#define PUSH(x) (*top++ = (x))
#define POP()   (*--top)

    for (;;) {
        Op_Code op = (Op_Code)*code++;
        switch (op) {
            case OP_HALT: {
                ASSERT(top - stack >= 1);
                return POP();
            } break;

            case OP_ADD: case OP_SUB:
            case OP_MUL: case OP_DIV: {
                ASSERT(top - stack >= 2);
                i32 b = POP();
                i32 a = POP();
                i32 res;
                switch (op) {
                    case OP_ADD: res = a + b; break;
                    case OP_SUB: res = a - b; break;
                    case OP_MUL: res = a * b; break;
                    case OP_DIV:
                        if (b == 0) { fatal("Division by zero"); }
                        res = a / b; break;
                    default: ASSERT_ALWAYS("Unreachable"); res = 0; break;
                }
                PUSH(res);
            } break;

            case OP_NEG: {
                ASSERT(top - stack >= 1);
                i32 a = POP();
                PUSH(-a);
            } break;

            case OP_LIT: {
                i32 value = *(i32*)code;
                code += sizeof(i32);
                PUSH(value);
            } break;
        }
    }

#undef PUSH
#undef POP
}

static void bytecode_generate_expr0(Parser_Context *p, u8 **out_code);

static void bytecode_generate_expr3(Parser_Context *p, u8 **out_code) {
    if (match_token(p, '(')) {
        bytecode_generate_expr0(p, out_code);
        expect_token(p, ')');
    } else if (is_token(p, TOK_INT_LITERAL)) {
        darray_add(*out_code, OP_LIT);
        i32 value = (i32)p->current.ivalue;
        u8 *value_bytes = (u8*)&value;
        for (usize i = 0; i < sizeof(i32); i++) {
            darray_add(*out_code, value_bytes[i]);
        }
        next_token(p);
    } else {
        fatal("Expected '(' or INT_LITERAL, got %d", p->current.kind);
    }
}

static void bytecode_generate_expr2(Parser_Context *p, u8 **out_code) {
    if (match_token(p, '-')) {
        bytecode_generate_expr2(p, out_code);
        darray_add(*out_code, OP_NEG);
    } else {
        bytecode_generate_expr3(p, out_code);
    }
}

static void bytecode_generate_expr1(Parser_Context *p, u8 **out_code) {
    bytecode_generate_expr2(p, out_code);
    for (;;) {
        if (match_token(p, '*')) {
            bytecode_generate_expr2(p, out_code);
            darray_add(*out_code, OP_MUL);
        } else if (match_token(p, '/')) {
            bytecode_generate_expr2(p, out_code);
            darray_add(*out_code, OP_DIV);
        } else {
            break;
        }
    }
}

static void bytecode_generate_expr0(Parser_Context *p, u8 **out_code) {
    bytecode_generate_expr1(p, out_code);
    for (;;) {
        if (match_token(p, '+')) {
            bytecode_generate_expr1(p, out_code);
            darray_add(*out_code, OP_ADD);
        } else if (match_token(p, '-')) {
            bytecode_generate_expr1(p, out_code);
            darray_add(*out_code, OP_SUB);
        } else {
            break;
        }
    }
}

static u8 *bytecode_generate_str(const char *source) {
    Parser_Context p = parser_init(source);
    u8 *code = NULL;
    bytecode_generate_expr0(&p, &code);
    ASSERT(match_token(&p, TOK_EOF), "Expected EOF, got %d", p.current.kind);
    darray_add(code, OP_HALT);
    return code;
}

#define TEST_BYTECODE(expr) \
    do { \
        u8* __code = bytecode_generate_str(#expr); \
        i32 __vm_res = vm_exec(__code); \
        ASSERT(__vm_res == (expr),  \
            "Bytecode for expression '%s' evaluated to %d, expected %d", #expr, __vm_res, (expr)); \
        fprintf(stdout, "Generated bytecode for expression '%s'.\n  > Execution result: %d\n", #expr, __vm_res); \
        darray_free(__code); \
    } while (0)

static void test_bytecode(void) {
    TEST_BYTECODE((2));
    TEST_BYTECODE(1 + 2);
    TEST_BYTECODE(1 + 2 * 3 + 5);
    TEST_BYTECODE(-1);
    TEST_BYTECODE(1 + 2 * 3 - 4 / 2 + (5 - 6) * 7);
    TEST_BYTECODE(42);
    TEST_BYTECODE((1 + 2) * (3 + 5));
    TEST_BYTECODE((-1000));
    TEST_BYTECODE((- -69));
}

#undef TEST_BYTECODE

int main(int argc, char *argv[]) {
    // test_darray(argc, argv);
    test_lex();
    test_str_intern();
    test_parser();
    test_bytecode();
    return 0;
}

#include "core.c"
