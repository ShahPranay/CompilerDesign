#include "AstNodes.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Value.h"
#include "llvm/Support/raw_ostream.h"

using namespace llvm;

LLVMContext* llvm_context;
Module* llvm_module;
IRBuilder<>* llvm_builder;
std::map<std::string, Value*> id_map;

static void initialize_module()
{
  llvm_context = new LLVMContext();
  llvm_module = new Module("C compiler", *llvm_context);
  
  llvm_builder = new IRBuilder<>(*llvm_context);
}

static void cleanup()
{
  delete llvm_builder;
  delete llvm_module;
  delete llvm_context;
}

Value *LogErrorV(const char *Str) {
  errs() << "Error: " << Str << "\n";
  return nullptr;
}

Value* IntegerExprAST::codegen() {
  return ConstantInt::get(*llvm_context, APInt(32, val, true));
}

Value* DoubleExprAST::codegen() {
  return ConstantFP::get(*llvm_context, APFloat(val));
}

Value* IdentifierAST::codegen() {
  Value* V = id_map[_name];
  if (!V) 
    return LogErrorV("Unknown Identifier name");
  return V;
}

Value* BinaryExprAST::codegen() {
  Value* L = left->codegen();
  Value* R = right->codegen();
  if (!L || !R)
    return nullptr;

  if (L->getType()->isIntegerTy(32) && R->getType()->isIntegerTy(32)) {
    if (op == "+")
      return llvm_builder->CreateAdd(L, R, "addtmp");
    if (op == "-")
      return llvm_builder->CreateSub(L, R, "subtmp");
    if (op == "*")
      return llvm_builder->CreateMul(L, R, "multmp");
    if (op == "/")
      return llvm_builder->CreateSDiv(L, R, "divtmp");
    if (op == "%")
      return llvm_builder->CreateSRem(L, R, "modtmp");
    if (op == "<")
      return llvm_builder->CreateICmpSLT(L, R, "cmptmp");
    if (op == ">")
      return llvm_builder->CreateICmpSGT(L, R, "cmptmp");
    if (op == "<=")
      return llvm_builder->CreateICmpSLE(L, R, "cmptmp");
    if (op == ">=")
      return llvm_builder->CreateICmpSGE(L, R, "cmptmp");
    if (op == "==")
      return llvm_builder->CreateICmpEQ(L, R, "cmptmp");
    if (op == "!=")
      return llvm_builder->CreateICmpNE(L, R, "cmptmp");
    if (op == "&")
      return llvm_builder->CreateAnd(L, R, "andtmp");
    if (op == "|")
      return llvm_builder->CreateOr(L, R, "ortmp");
    if (op == "^")
      return llvm_builder->CreateXor(L, R, "xortmp");
    if (op == "<<")
      return llvm_builder->CreateShl(L, R, "shltmp");
    if (op == ">>")
      return llvm_builder->CreateAShr(L, R, "ashrtmp");
  } else if (L->getType()->isDoubleTy() && R->getType()->isDoubleTy()) {
    if (op == "+")
      return llvm_builder->CreateFAdd(L, R, "addtmp");
    if (op == "-")
      return llvm_builder->CreateFSub(L, R, "subtmp");
    if (op == "*")
      return llvm_builder->CreateFMul(L, R, "multmp");
    if (op == "/")
      return llvm_builder->CreateFDiv(L, R, "divtmp");
    if (op == "<")
      L = llvm_builder->CreateFCmpULT(L, R, "cmptmp");
      return llvm_builder->CreateUIToFP(L, Type::getDoubleTy(*llvm_context), "booltmp");
    if (op == ">")
      L = llvm_builder->CreateFCmpUGT(L, R, "cmptmp");
      return llvm_builder->CreateUIToFP(L, Type::getDoubleTy(*llvm_context), "booltmp");
    if (op == "<=")
      L = llvm_builder->CreateFCmpULE(L, R, "cmptmp");
      return llvm_builder->CreateUIToFP(L, Type::getDoubleTy(*llvm_context), "booltmp");
    if (op == ">=")
      L = llvm_builder->CreateFCmpUGE(L, R, "cmptmp");
      return llvm_builder->CreateUIToFP(L, Type::getDoubleTy(*llvm_context), "booltmp");
    if (op == "==")
      L = llvm_builder->CreateFCmpUEQ(L, R, "cmptmp");
      return llvm_builder->CreateUIToFP(L, Type::getDoubleTy(*llvm_context), "booltmp");
    if (op == "!=")
      L = llvm_builder->CreateFCmpUNE(L, R, "cmptmp");
      return llvm_builder->CreateUIToFP(L, Type::getDoubleTy(*llvm_context), "booltmp");
  }

  return LogErrorV("Invalid binary operator");
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
