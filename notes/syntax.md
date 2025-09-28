# Lexer syntax

Note this is mostly for reference. This is not a very formal specification yet.

- Tokens

`( ) [ ] { }`

* Unary operators (prefix)
`+ - ! ~ & * ?`

* Unary operators (postfix)
`.* .`
    * Not yet sure if `.*` should be used for deference. Maybe change it

* Binary operators (all left associative except for assignment and ternary)

```
* / %                                               < Left associative
+ -                                                 < Left associative
<< >>                                               < Left associative
== != < > <= >=                                     < Left associative
&                                                   < Left associative
^                                                   < Left associative
|                                                   < Left associative
&&                                                  < Left associative
||                                                  < Left associative
= += -= *= /= %= <<= >>= &= ^= |= &&= ||= := ::     < Right associative
? :                                                 < Right associative
```

Operators that are more than a character long need names:

```
==  EQ
!=  NEQ
<=  LTEQ
>=  GTEQ
<<  LSHIFT
>>  RSHIFT
&&  AND
||  OR
+=  ADD_ASSIGN
-=  SUB_ASSIGN
*=  MUL_ASSIGN
/=  DIV_ASSIGN
%=  MOD_ASSIGN
<<= LSHIFT_ASSIGN
>>= RSHIFT_ASSIGN
&=  BIT_AND_ASSIGN
^=  BIT_XOR_ASSIGN
|=  BIT_OR_ASSIGN
&&= AND_ASSIGN
||= OR_ASSIGN
::  CONST_ASSIGN
:=  LET_ASSIGN
.*  DEREF
.?  NULLABLE_ACCESS
++  INC
--  DEC
```


* Integer literals

```[0-9]+ | 0[xX][0-9a-fA-F]+ | 0[bB][01]+ | 0[oO][0-7]+```

Really the integer can contain `_` as a separator between digits (not at the start or end or just after the prefix).
The regex gets a bit more complicated, but keep that in mind.

For example, a decimal integer regex should be:
```
[0-9]([0-9_]*[0-9])?
```

This is here for explanation purpose, we don't really use regex in the lexer.

* Floating point literals

```
FLT_LIT:            DECIMAL_FLT | HEXADECIMAL_FLT
DECIMAL_FLOAT:      [0-9]*.[0-9]+([eE][+-]?[0-9]+)? | [0-9]+.[0-9]*([eE][+-]?[0-9]+)? | [0-9]+[eE][+-]?[0-9]+
HEXADECIMAL_FLT:    0[xX]([0-9a-fA-F]*.[0-9a-fA-F]+ | [0-9a-fA-F]+.[0-9a-fA-F]*) [pP][+-]?[0-9]+
```

* Names (identifiers)

```
[a-zA-Z_][a-zA-Z0-9_]*
```

* Comments

```'/' '/' .*```

```'/' '*' ( . | '*' '/' | '*' )* '*' '/'```

Note that we support nested block comments. When translating to C, if we want to preserve comments, we need
to turn every inner `*/` into `* /` to avoid ending the comment too early.

* String literals

```
'"' [^"]* '"'
```

* Character literals (TODO: support escape sequences)

```
'\'' ( '\\'[nrtvba] | [^'\\] ) '\''
