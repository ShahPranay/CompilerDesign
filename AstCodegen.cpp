#include "AstNodes.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Value.h"
#include "llvm/Support/raw_ostream.h"

#include<stack>
#include<map>

using namespace llvm;

LLVMContext* llvm_context;
Module* llvm_module;
IRBuilder* llvm_builder;
std::map<std::string, AllocaInst*> global_symbols;
std::vector<std::map<std::string, AllocaInst*>> nested_symbols;

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
  return ConstantInt::get(*llvm_context, APInt(32, _val, true));
}

Value* DoubleExprAST::codegen() {
  return ConstantFP::get(*llvm_context, APFloat(_val));
}

Value* IdentifierAST::codegen() {
  AllocaInst* A = nullptr;
  for (int i = nested_symbols.size() - 1; i >= 0; i--){
    if (nested_symbols[i].find(_name) != nested_symbols[i].end())
    {
      A = nested_symbols[i][_name];
      break;
    }
  }

  if (!A)
    A = global_symbols[_name];

  if (!A) 
    return LogErrorV("Unknown Identifier name");

  return llvm_builder->createLoad(A->getAllocatedType(), A, _name.c_str());
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

Type* DeclSpecifiersAST::getLLVMType()
{
  if(_decl_specs.size() != 1)
    return LogErrorV("Number of specifiers do not match 1");

  SpecifierAST* spec = _decl_specs.begin();
  PrimitiveTypeSpecAST* prim_spec = dynamic_cast<PrimitiveTypeSpecAST*>(spec);
  if(!prim_spec)
    LogErrorV("Not a primitive Type Specifier");
  
  std::string type_name = prim_spec->getName();
  Type* ret_type = nullptr;

  if(type_name == "int")
    ret_type = Type::getInt32Ty(llvm_context);
  else if(type_name == "char")
    ret_type = Type::getInt8Ty(llvm_context);
  else if(type_name == "double")
    ret_type = Type::getDoubleTy(llvm_context);
  else 
    return LogErrorV("Invalid Type");

  return ret_type;
}

std::vector<Type*> ParamListAST::getParamTypes()
{
  std::vector<Type*> params;
  for (auto param: _params)
  {
    params.push_back(param->getLLVMType()); 
  }

  return params;
}

Function* FunctionDefinitionAST::codegen()
{
  
}

void NormalDeclAST::codegen()
{
  if(_init_decl_list.size() == 1)
  {
    FunctionDeclaratorAST* fn = dynamic_cast<FunctionDeclaratorAST*>(_init_decl_list.begin());  
    if(fn)
      NormalDeclAST::declareLLVMFunction(_specs->getLLVMType(), fn->getFnName(), fn->getParamTypes());
  }
  // handle variable declarations
}

static Function* NormalDeclAST::declareLLVMFunction(Type* ret_type, std::string name, std::vector<Type*> param_types)
{
  // modify for ellipsis
  FunctionTy* FT = FunctionType::get(ret_type, param_types, false);
  Function* F = Function::Create(FT, Function::ExternalLinkage, name, llvm_module.get());

  // set arg names
}
