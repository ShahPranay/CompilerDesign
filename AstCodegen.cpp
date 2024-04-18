#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Value.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/IR/Verifier.h"

#include "AstNodes.h"

#include<stack>
#include<map>
#include <iostream>

using std::cout, std::endl;

using namespace llvm;

LLVMContext* llvm_context;
Module* llvm_module;
IRBuilder<>* llvm_builder;
std::map<std::string, AllocaInst*> global_symbols;
std::vector<std::map<std::string, AllocaInst*>> nested_symbols;

void initialize_module()
{
  llvm_context = new LLVMContext();
  llvm_module = new Module("C compiler", *llvm_context);
  
  llvm_builder = new IRBuilder<>(*llvm_context);
}

void cleanup_module()
{
  delete llvm_builder;
  delete llvm_module;
  delete llvm_context;
}

void LogErrorV(const char *Str) {
  errs() << "Error: " << Str << "\n";
}

static AllocaInst *CreateEntryBlockAlloca(Function *F, StringRef var_name, Type *type)
{
  IRBuilder<> TmpB(&F->getEntryBlock(), F->getEntryBlock().begin());

  return TmpB.CreateAlloca(type, nullptr, var_name);
}

Value* IntegerExprAST::codegen() {
  return ConstantInt::get(*llvm_context, APInt(32, _val, true));
}

Value* DoubleExprAST::codegen() {
  return ConstantFP::get(*llvm_context, APFloat(_val));
}

AllocaInst* IdentifierAST::getAlloca()
{
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
  {
    LogErrorV("Unknown Identifier name");
  }

  return A;
}

Value* IdentifierAST::codegen() 
{
  AllocaInst *A = getAlloca();
  return llvm_builder->CreateLoad(A->getAllocatedType(), A, _name.c_str());
}

Value* BinaryExprAST::codegen() {

  if (op == "=")
  {
    IdentifierAST *LHSE = static_cast<IdentifierAST*>(left);
    if (!LHSE){
      LogErrorV("destination of \'=\' must be a variable.");
      return nullptr;
    }
    
    Value *R = right->codegen();
    if (!R)
      return nullptr;
    
    Value *VarValue = LHSE->getAlloca(); 
    if(!VarValue)
    {
      LogErrorV("Unknown variable name.");
      return nullptr;
    }
    
    llvm_builder->CreateStore(R, VarValue);
    return R;
  }

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

  LogErrorV("Invalid binary operator");
  return nullptr;
}

Value* ExprStmtAST::codegen()
{
  return _expression->codegen();
}

Value* IfElseStmtAST::codegen()
{
  Value* CondV = _expression->codegen();
  if (!CondV) {
    LogErrorV("Failed to generate code for if condition");
    return nullptr;
  }

  if (!CondV->getType()->isIntegerTy(1)) {
      LogErrorV("If condition must be of type boolean");
      return nullptr;
  }

  Function* TheFunction = llvm_builder->GetInsertBlock()->getParent();
  BasicBlock* ThenBB = BasicBlock::Create(*llvm_context, "then", TheFunction);
  BasicBlock* ElseBB = BasicBlock::Create(*llvm_context, "else");
  BasicBlock* MergeBB = BasicBlock::Create(*llvm_context, "ifcont");

  llvm_builder->CreateCondBr(CondV, ThenBB, ElseBB);

  llvm_builder->SetInsertPoint(ThenBB);
  Value* ThenV = _then_statement->codegen();
  if (!ThenV)
  {
      LogErrorV("Failed to generate code for then clause");
      return nullptr;
  }
  llvm_builder->CreateBr(MergeBB);

  ThenBB = llvm_builder->GetInsertBlock();

  TheFunction->getBasicBlockList().push_back(ElseBB);
  llvm_builder->SetInsertPoint(ElseBB);
  Value* ElseV = nullptr;
  if (_else_statement)
  {
      ElseV = _else_statement->codegen();
      if (!ElseV)
      {
          LogErrorV("Failed to generate code for else clause");
          return nullptr;
      }
  }
  llvm_builder->CreateBr(MergeBB);

  ElseBB = llvm_builder->GetInsertBlock();

  TheFunction->getBasicBlockList().push_back(MergeBB);
  llvm_builder->SetInsertPoint(MergeBB);
  PHINode* PN = llvm_builder->CreatePHI(Type::getVoidTy(*llvm_context), 2, "iftmp");
  PN->addIncoming(ThenV, ThenBB);
  if (_else_statement)
  {
      PN->addIncoming(ElseV, ElseBB);
  }

  return PN;
}

Value* ReturnStmtAST::codegen()
{
  if (_expr) {
      Value* RetVal = _expr->codegen();
      if (!RetVal) {
          LogErrorV("Failed to generate code for return expression");
          return nullptr;
      }

      Function* TheFunction = llvm_builder->GetInsertBlock()->getParent();
      if (!TheFunction->getReturnType()->isVoidTy() && RetVal->getType() != TheFunction->getReturnType()) {
          LogErrorV("Return type mismatch");
          return nullptr;
      }

      return llvm_builder->CreateRet(RetVal);
  } else {
      Function* TheFunction = llvm_builder->GetInsertBlock()->getParent();
      if (!TheFunction->getReturnType()->isVoidTy()) {
          LogErrorV("Return type mismatch");
          return nullptr;
      }

      return llvm_builder->CreateRetVoid();
  }
}

Value* GotoStmtAST::codegen()
{
  LogErrorV("goto Not implemented yet");
  return nullptr;
}

Value* WhileStmtAST::codegen()
{
  Function *TheFunction = llvm_builder->GetInsertBlock()->getParent();
  BasicBlock *CondBlock = BasicBlock::Create(*llvm_context, "while.cond", TheFunction);
  BasicBlock *LoopBlock = BasicBlock::Create(*llvm_context, "while.loop");
  BasicBlock *AfterBlock = BasicBlock::Create(*llvm_context, "while.after");

  llvm_builder->CreateBr(CondBlock);

  llvm_builder->SetInsertPoint(CondBlock);

  Value *ConditionValue = _expression->codegen();
  if (!ConditionValue) {
    LogErrorV("Failed to generate code for condition");
    return nullptr;
  }

  llvm_builder->CreateCondBr(ConditionValue, LoopBlock, AfterBlock);

  TheFunction->getBasicBlockList().push_back(LoopBlock);
  llvm_builder->SetInsertPoint(LoopBlock);
  _statement->codegen();

  llvm_builder->CreateBr(CondBlock);

  TheFunction->getBasicBlockList().push_back(AfterBlock);
  llvm_builder->SetInsertPoint(AfterBlock);

  return nullptr;
}

void RootAST::codegen()
{
  for(auto ed: _extern_units)
  {
    ed->codegen();
  }
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
  {
    LogErrorV("Number of specifiers do not match 1");
    return nullptr;
  }

  SpecifierAST* spec = *_decl_specs.begin();
  PrimitiveTypeSpecAST* prim_spec = dynamic_cast<PrimitiveTypeSpecAST*>(spec);
  if(!prim_spec)
  {
    LogErrorV("Not a primitive Type Specifier");
    return nullptr;
  }
  
  std::string type_name = prim_spec->getName();
  Type* ret_type = nullptr;

  if(type_name == "int")
    ret_type = Type::getInt32Ty(*llvm_context);
  else if(type_name == "char")
    ret_type = Type::getInt8Ty(*llvm_context);
  else if(type_name == "double")
    ret_type = Type::getDoubleTy(*llvm_context);
  else if(type_name == "void")
    ret_type = Type::getVoidTy(*llvm_context);
  else 
  {
    LogErrorV("Invalid Type");
    return nullptr;
  }

  return ret_type;
}

std::vector<Type*> ParamListAST::getParamTypes()
{
  std::vector<Type*> types;
  for (auto param: _params)
  {
    types.push_back(param->getLLVMType()); 
  }

  return types;
}

std::vector<std::string> ParamListAST::getParamNames()
{
  std::vector<std::string> names;
  for (auto param: _params)
  {
    names.push_back(param->getName());
  }
  return names;
}



void FunctionDefinitionAST::codegen()
{
  Function *F = llvm_module->getFunction(_func_declarator->getName());

  if(!F)
    _func_declarator->codegen(_decl_specs->getLLVMType());

  F = llvm_module->getFunction(_func_declarator->getName());

  if(!F)
    return;

  if(!F->empty()){
    LogErrorV("Cannot be redefined");
    return;
  }

  BasicBlock *BB = BasicBlock::Create(*llvm_context, "entry", F);
  llvm_builder->SetInsertPoint(BB);

  nested_symbols.clear();
  nested_symbols.push_back({});
  for (auto &arg: F->args())
  {
    AllocaInst *Alloca = CreateEntryBlockAlloca(F, arg.getName(), arg.getType());
    llvm_builder->CreateStore(&arg, Alloca);
    nested_symbols[0][std::string(arg.getName())] = Alloca;
  }

  // body codgen
  _compound_stmts->codegen();
  verifyFunction(*F);
}

void NormalDeclAST::codegen()
{
  _init_decl_list->codegen(_specs->getLLVMType());
}

void InitDeclaratorListAST::codegen(Type* specifier_type)
{
  for (auto idecl: _init_declarators)
  {
    idecl->codegen(specifier_type);
  }
}

void InitDeclaratorAST::codegen(Type* specifier_type)
{
  _direct_decl->codegen(specifier_type);

  // handle initializations
}

void FunctionDeclaratorAST::codegen(Type* specifier_type)
{
  // incorporate ellipsis
  auto param_types = _paramlist->getParamTypes();
  auto param_names = _paramlist->getParamNames();
  
  FunctionType* FT = FunctionType::get(specifier_type, param_types , false);
  Function* F = Function::Create(FT, Function::ExternalLinkage, _identifier->getName(), *llvm_module);

  unsigned int i = 0;
  for (auto &Arg: F->args())
  {
    Arg.setName(param_names[i++]);
  }
}

void IdDeclaratorAST::codegen(Type* specifier_type)
{
  // Check if the identifier has already been declared in the current scope
    if (nested_symbols.back().find(_name) != nested_symbols.back().end())
    {
        LogErrorV("Redefinition of variable: ");
        return;
    }

    // Create an alloca instruction for the variable
    AllocaInst* Alloca = llvm_builder->CreateAlloca(specifier_type, nullptr, _name);

    // Add the variable to the current symbol table
    nested_symbols.back()[_name] = Alloca;

    // Print the name of the variable (optional)
    cout << "Declared variable: " << _name << endl;
}
