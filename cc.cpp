#include <stdio.h>
#include <stdlib.h>
#include <string>
#include <assert.h>
#include <system_error>
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

void codegen(char const *filename)
{
  initialize_module();
  AST_root->codegen();

  std::string outfilename(filename);
  int e = outfilename.size() - 1;
  while(e >= 0 && outfilename[e] != '.')
  {
    outfilename.pop_back();
    e--;
  }
  outfilename.pop_back();
  outfilename += "_cc.ll";

  std::error_code EC;
  llvm::raw_fd_ostream outstream(outfilename, EC);
  llvm_module->print(outstream, nullptr);
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

  AST_root->constantFolding();
  AST_root->localDeadCodeElim();
  dump_ast();
  codegen(filename);

  exit(0);
}
