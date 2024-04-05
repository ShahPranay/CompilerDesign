#include "AstNodes.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/LLVMContext.h"

using namespace llvm;

LLVMContext* llvm_context;
Module* llvm_module;
IRBuilder* llvm_builder;
std::map<std::string, Value*> id_map;

static void initialize_module()
{
  llvm_context = new LLVMContext();
  llvm_module = new Module("C compiler", *llvm_context);
  
  llvm_builder = new IRBuilder(*llvm_context);
}

static void cleanup()
{
  delete llvm_builder;
  delete llvm_module;
  delete llvm_context;
}

Value* FunctionDefinitionAST::codegen()
{
  return _compound_stmts->codegen();
}

Value* BlockItemListAST::codegen()
{
  Value* laststmt;
  for(auto bi: _items)
  {
    laststmt = bi->codegen();
  }
  return laststmt;
}
