#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include "AstNodes.h"
#include <iostream>
#include "c.tab.hpp"
#include "llvm/IR/Module.h"
#include "llvm/Support/raw_ostream.h"

using namespace std;

extern "C" int yylex();
int yyparse();
extern "C" FILE *yyin;

extern std::unique_ptr<RootAST> AST_root;

extern llvm::Module* llvm_module;
void initialize_module();
void cleanup_module();

static void usage()
{
  printf("Usage: cc <prog.c>\n");
}

void dump_ast() {
  AST_root->print(0);
}

void codegen()
{
  initialize_module();
  AST_root->codegen();
  llvm_module->print(llvm::errs(), nullptr);
  cleanup_module();
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

  int ret = yyparse();

  printf("retv = %d\n", ret);

  dump_ast();
  codegen();

  exit(0);
}
