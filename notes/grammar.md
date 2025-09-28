# GRIM grammar

This document semi-formally describes the GRIM programming language grammar.

It is more of a reference than a specification for the time being.


```

ident          = ( letter | '_' ) { letter | digit | '_' }
letter         = 'a'..'z' | 'A'..'Z'
digit          = '0'..'9'

base-type      = 'u8' | 'u16' | 'u32' | 'u64'
               | 'i8' | 'i16' | 'i32' | 'i64'
               | 'f32' | 'f64'
               | 'isize' | 'usize'
               | 'bool' | 'b8' | 'b16' | 'b32' | 'b64'
               | 'string'
               | ident

type           = [ { '*' [ 'const' ] | '?' | '[' expr ']' } ] base-type

aggregate-decl = ident [ { ',' ident } ] ':' type [ '=' expr ]
enum-member    = ident [ '=' expr ]
func-param     = ident [ { ',' ident } ] ':' type [ '=' expr ]

struct-decl    = 'struct' aggregate-decl ';'
enum-decl      = 'enum' ['@flag'] [ int-type ] '{' enum-member { ',' enum-member } [ ',' ] '}' ';'
union-decl     = 'union' ['@packed'] aggregate-decl ';'
func-decl      = 'proc' '(' func-param-list ')' [ type ] ( stmt-block | '=>' stmt ';' )

type-decl      = struct-decl | enum-decl | union-decl | func-decl

decl           = const-decl | var-decl

const-decl     = '::' ( expr | type-decl ) ';'
               | ':' type [ ':' expr ] ';'

assign-op      = '=' | '+=' | '-=' | '*=' | '/=' | '%='
                | '<<=' | '>>=' | '&=' | '^=' | '|='
                | '&&=' | '||='
                | ':=' | '::'

range           = expr '..' ( '<' | '=' ) expr

switch-case     = 'case' ( expr | range ) [ ',' ( expr | range ) ] ':'

stmt-block      = '{' { stmt } '}'

switch-block    = '{' { switch-case ':' stmt } [ 'default' ':' { stmt } ] '}'

switch-stmt     = 'switch' expr switch-block

label-stmt      = ident ':' ( stmt-block | loop-stmt | switch-stmt )

loop-stmt       = for-stmt
                | 'while' expr stmt-block
                | 'do' stmt-block 'while' expr ';'

expr-stmt       = expr ';'
                | expr assign-op expr ';'

// @TODO: for
stmt            = 'return' expr ';'
                | 'break' [ ':' label ] ';'
                | 'continue' [ ':' label ] ';'
                | stmt-block
                | 'if' expr stmt-block [ 'else' stmt-block ]
                | loop-stmt
                | switch-stmt
                | expr-stmt
                | label-stmt

expr           = bin-op-expr | un-op-expr | expr-list | primary-expr | ternary-expr
bin-op-expr    = expr bin-op expr
un-op-expr     = un-op expr
ternary-expr   = expr '?' expr ':' expr
expr-list      = expr { ',' expr }
primary-expr   = literal
               | ident
               | '(' expr ')'
               | cast-expr
               | func-call
               | field-access
               | array-access
               | pointer-deref
               | nullable-access
               | '++' expr
               | '--' expr
               | expr '++'
               | expr '--'
               | sizeof-expr
               | alignof-expr
               | offsetof-expr
               | typeof-expr

cast-expr      = 'cast(' type ')' expr

func-call      = expr '(' [ expr { ',' expr } [,] ] ')'
field-access   = expr ( '.' ident )

pointer-deref  = expr '.*'
nullable-access= expr '.?'

```
