//
// Entry point for the grim-lang compiler
//

#include "common.h"
#include "tok.h"
#include "lex.h"
#include "ast.h"
#include "parse.h"

int main(int argc, char *argv[]) {
    // DO_TEST(common);
    // DO_TEST(lex);
    // DO_TEST(ast);
    DO_TEST(parse);
}

#include "common.c"
#include "tok.c"
#include "lex.c"
#include "ast.c"
#include "parse.c"
