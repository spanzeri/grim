#include "lex.h"

// Needs to be here for float parsing
// @TODO: Remove once we have our own float parser
#include <errno.h>
#include <math.h>

typedef enum Numeric_Parse_State {
    OK,
    INVALID,
    OVERFLOW,
} Numeric_Parse_State;

static bool is_digit(u32 c);
static u32  lexer_peek_codepoint(Lexer_Context *l);
static u32  lexer_read_codepoint(Lexer_Context *l);
static bool lexer_match_codepoint(Lexer_Context *l, u32 expected);
static u64  lexer_scan_integer(Lexer_Context *l, u32 base, Numeric_Parse_State* state);
static void lexer_scan_numeric_literal(Lexer_Context *l, Token *tok);
static void lexer_scan_char(Lexer_Context* l, Token* tok);
static void lexer_scan_string(Lexer_Context* l, Token* tok);
static void lexer_scan_name_or_keyword(Lexer_Context* l, Token* tok);

Lexer_Context lexer_init(const char *source) {
    return (Lexer_Context){
        .source     = source,
        .at         = source,
    };
}

Token lexer_next_token(Lexer_Context *l) {
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
        CASE2(':', '=', TOK_VAR_ASSIGN, ':', TOK_CONST_ASSIGN);

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

        case '{': case '}':
        case '[': case ']':
        case '(': case ')':
        case '?': case ';':
        case ',': {
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
            lexer_scan_name_or_keyword(l, &tok);
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

static bool is_digit(u32 c) {
    return c >= '0' && c <= '9';
}

static u64 lexer_scan_integer(Lexer_Context *l, u32 base, Numeric_Parse_State* state) {
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

    u64 integer_part = lexer_scan_integer(l, base, &parse_state);
    u32 c = lexer_peek_codepoint(l);
    bool is_float = (base == 10 || base == 16) && c == '.';
    is_float |= (base == 10) && (c == 'e' || c == 'E');
    is_float |= (base == 16) && (c == 'p' || c == 'P');

    if (is_float) {
        // @TODO: We rely on the standard library here to parse a floating-point
        // number. We should roll out our own implementation at some point.

        if (c == '.') { lexer_read_codepoint(l); }
        const char* frac_start = l->at;
        lexer_scan_integer(l, base, &parse_state);
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
            lexer_scan_integer(l, 10, &parse_state);
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

const String kw_strings[] = {
    [KW_PROC]       = str_from_lit("proc"),
    [KW_RETURN]     = str_from_lit("return"),
    [KW_IF]         = str_from_lit("if"),
    [KW_ELSE]       = str_from_lit("else"),
    [KW_WHILE]      = str_from_lit("while"),
    [KW_FOR]        = str_from_lit("for"),
    [KW_DO]         = str_from_lit("do"),
    [KW_SWITCH]     = str_from_lit("switch"),
    [KW_BREAK]      = str_from_lit("break"),
    [KW_CONTINUE]   = str_from_lit("continue"),
    [KW_CONST]      = str_from_lit("const"),
    [KW_ENUM]       = str_from_lit("enum"),
    [KW_STRUCT]     = str_from_lit("struct"),
    [KW_UNION]      = str_from_lit("union"),
    [KW_CAST]       = str_from_lit("cast"),
    [KW_SIZEOF]     = str_from_lit("sizeof"),
    [KW_ALIGNOF]    = str_from_lit("alignof"),
    [KW_TRUE]       = str_from_lit("true"),
    [KW_FALSE]      = str_from_lit("false"),
    [KW_NULL]       = str_from_lit("null"),
};

static void lexer_scan_name_or_keyword(Lexer_Context* l, Token* tok) {
    for (;;) {
        u32 c2 = lexer_peek_codepoint(l);
        if (!((c2 >= 'a' && c2 <= 'z') || (c2 >= 'A' && c2 <= 'Z') || is_digit(c2) || c2 == '_')) {
            break;
        }
        lexer_read_codepoint(l);
    }

    // Check for keywords
    String name = str_from_range(tok->start, l->at);
    if (name.len >= 2) {
        for (size_t i = 0; i < COUNTOF(kw_strings); i++) {
            if (str_eq(name, kw_strings[i])) {
                tok->kind    = TOK_KEYWORD;
                tok->keyword = (Keyword)i;
                return;
            }
        }
    }

    tok->kind = TOK_IDENTIFIER;
    tok->name = str_intern_range(tok->start, l->at);
}

TEST(lex) {
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
        "Got %d tokens, expected %zu", darray_len(tokens), COUNTOF(expected_tokens));
    for (int i = 0; i < darray_len(tokens); i++) {
        ASSERT(tokens[i].kind == expected_tokens[i].kind,
            "Token %d: got kind %d, expected %d", i, tokens[i].kind, expected_tokens[i].kind);
        if (tokens[i].kind == TOK_INT_LITERAL) {
            ASSERT(tokens[i].ivalue == expected_tokens[i].ivalue,
                "Token %d: got int_value %llu, expected %llu", i,
                tokens[i].ivalue, expected_tokens[i].ivalue);
        }
        else if (tokens[i].kind == TOK_IDENTIFIER) {
            ASSERT(
                tokens[i].name == str_intern(expected_tokens[i].start),
                "Token %d: got identifier '%s', expected '%s'", i,
                tokens[i].name, expected_tokens[i].start);
        }
    }

    ASSERT(tokens[1].name == tokens[4].name,
        "Identifier interning failed: tokens[1] = '%s', tokens[4] = '%s'",
        tokens[1].name, tokens[4].name);
    ASSERT(strcmp(tokens[1].name, "XY") == 0,
        "Identifier interning failed: tokens[1] = '%s', expected 'XY'",
        tokens[1].name);

    for (int i = 0; i < darray_len(tokens); i++) {
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
        strncmp(tok.svalue, (expected), (size_t)darray_len(tok.svalue) - 1) == 0                                \
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
    TEST_TOK_KIND(TOK_VAR_ASSIGN);
    TEST_TOK_KIND(TOK_CONST_ASSIGN);
    TEST_TOK_KIND(TOK_EOF);

#undef TEST_TOK_KIND
#undef TEST_STRING_LITERAL
#undef TEST_CHAR_LITERAL
#undef TEST_FLT_LITERAL
#undef TEST_INT_LITERAL
}

