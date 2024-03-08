#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include "AstNodes.h"
#include <iostream>
#include "c.tab.hpp"

using namespace std;

extern "C" int yylex();
int yyparse();
extern "C" FILE *yyin;

extern std::unique_ptr<RootAST> AST_root;

static void usage()
{
  printf("Usage: cc <prog.c>\n");
}

void dump_ast() {
  AST_root->print(0);
}

int
main(int argc, char **argv)
{
  if (argc != 2) {
    usage();
    exit(1);
  }
  char const *filename = argv[1];
  yyin = fopen(filename, "r");
  assert(yyin);
  std::cout << "before" << endl;
  int ret = yyparse();
  std::cout << "after" << endl;
  dump_ast();
  printf("retv = %d\n", ret);
  exit(0);
}
