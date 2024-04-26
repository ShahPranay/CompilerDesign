#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Value.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/IR/Verifier.h"

#include "AstNodes.h"

#include <stack>
#include <map>
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
    cout << _name << endl;
    LogErrorV("Unknown Identifier name");
  }

  return A;
}

Value* StrLiteralAST::codegen() {
  /* ArrayType *strtype = ArrayType::get(Type::getInt8Ty(*llvm_context), _str.size()); */
  /* GlobalVariable *globalvar = new GlobalVariable(*llvm_module, strtype, false, GlobalValue::ExternalLinkage, nullptr, ""); */
  /* globalvar->setInitializer(ConstantDataArray::getString(*llvm_context, _str, false)); */
  /* globalvar->setConstant(true); */

  /* globalvar->setAlignment(MaybeAlign(1)); */

  return llvm_builder->CreateGlobalString(_str, "str");
}

Value* IdentifierAST::codegen() {
  AllocaInst *A = getAlloca();
  if (!A) {
    return nullptr;  // If allocation instruction not found, return nullptr
  }
  return llvm_builder->CreateLoad(A->getAllocatedType(), A, _name.c_str());
}

Value* UnaryExprAST::codegen() {
  llvm::Value* val = _expr->codegen();
  
  if (!val) {
    return nullptr;
  }

  if (_op == "&") {
    //Todo
  } else if (_op == "*") {
    if (!val->getType()->isPointerTy()) {
      LogErrorV("Cannot dereference non-pointer type");
      return nullptr;
    }
    /* return llvm_builder->CreateLoad(val); */
  } else if (_op == "+") {
    return val;
  } else if (_op == "-") {
    return llvm_builder->CreateNeg(val);
  } else if (_op == "~") {
    return llvm_builder->CreateNot(val);
  } else if (_op == "!") {
    llvm::Type *boolTy = llvm::Type::getInt1Ty(*llvm_context);
    return llvm_builder->CreateICmpEQ(val, llvm::Constant::getNullValue(boolTy));
  } else if (_op == "++") {
    if (!val->getType()->isIntegerTy()) {
        LogErrorV("Cannot decrement non-integer type");
        return nullptr;
    }
    Value *one = llvm::ConstantInt::get(val->getType(), 1);
    return llvm_builder->CreateAdd(val, one, "increment");
  } else if (_op == "--") {
    if (!val->getType()->isIntegerTy()) {
        LogErrorV("Cannot decrement non-integer type");
        return nullptr;
    }
    Value *one = llvm::ConstantInt::get(val->getType(), 1);
    return llvm_builder->CreateSub(val, one, "decrement");
  } else {
    LogErrorV("Invalid unary operator");
    return nullptr;
  }
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

  Value *L, *R;
  L = left->codegen();
  R = right->codegen();
  if (!L || !R)
    return nullptr;

  if(op == "||")
  {
    if (L->getType()->isIntegerTy(1) && R->getType()->isIntegerTy(1))
      return llvm_builder->CreateOr(L, R, "cmptmp"); 
  }
  else if(op == "&&")
  {
    if (L->getType()->isIntegerTy(1) && R->getType()->isIntegerTy(1))
      return llvm_builder->CreateAnd(L, R, "cmptmp"); 
  }
  else if (L->getType()->isIntegerTy(32) && R->getType()->isIntegerTy(32)) {
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

void ExprStmtAST::codegen()
{
  _expression->codegen();
}

void IfElseStmtAST::codegen()
{
  Value* CondV = _expression->codegen();
  if (!CondV) {
    LogErrorV("Failed to generate code for if condition");
    return ;
  }

  if (!CondV->getType()->isIntegerTy(1)) {
      LogErrorV("If condition must be of type boolean");
      return ;
  }

  Function* TheFunction = llvm_builder->GetInsertBlock()->getParent();
  BasicBlock* ThenBB = BasicBlock::Create(*llvm_context, "then", TheFunction);
  BasicBlock* ElseBB = BasicBlock::Create(*llvm_context, "else");
  BasicBlock* MergeBB = BasicBlock::Create(*llvm_context, "ifcont");

  llvm_builder->CreateCondBr(CondV, ThenBB, ElseBB);

  llvm_builder->SetInsertPoint(ThenBB);
  _then_statement->codegen();

  llvm_builder->CreateBr(MergeBB);

  ThenBB = llvm_builder->GetInsertBlock();

  TheFunction->insert(TheFunction->end(), ElseBB);
  llvm_builder->SetInsertPoint(ElseBB);

  if (_else_statement)
      _else_statement->codegen();

  llvm_builder->CreateBr(MergeBB);

  ElseBB = llvm_builder->GetInsertBlock();

  TheFunction->insert(TheFunction->end(), MergeBB);
  llvm_builder->SetInsertPoint(MergeBB);
  /* PHINode* PN = llvm_builder->CreatePHI(Type::getVoidTy(*llvm_context), 2, "iftmp"); */
  /* PN->addIncoming(ThenV, ThenBB); */
  /* if (_else_statement) */
  /* { */
  /*     PN->addIncoming(ElseV, ElseBB); */
  /* } */

  return ;
}

void ReturnStmtAST::codegen()
{
  if (_expr) {
      Value* RetVal = _expr->codegen();
      if (!RetVal) {
          LogErrorV("Failed to generate code for return expression");
          return;
      }

      Function* TheFunction = llvm_builder->GetInsertBlock()->getParent();
      if (!TheFunction->getReturnType()->isVoidTy() && RetVal->getType() != TheFunction->getReturnType()) {
          LogErrorV("Return type mismatch");
          return;
      }

      llvm_builder->CreateRet(RetVal);
  } else {
      Function* TheFunction = llvm_builder->GetInsertBlock()->getParent();
      if (!TheFunction->getReturnType()->isVoidTy()) {
          LogErrorV("Return type mismatch");
          return;
      }


      llvm_builder->CreateRetVoid();
  }
}

void GotoStmtAST::codegen()
{
  LogErrorV("goto Not implemented yet");
  return;
}

void WhileStmtAST::codegen()
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
    return;
  }

  llvm_builder->CreateCondBr(ConditionValue, LoopBlock, AfterBlock);


  TheFunction->insert(TheFunction->end(), LoopBlock);
  llvm_builder->SetInsertPoint(LoopBlock);
  _statement->codegen();

  llvm_builder->CreateBr(CondBlock);

  TheFunction->insert(TheFunction->end(), AfterBlock);
  llvm_builder->SetInsertPoint(AfterBlock);
}

Value* FunctionCallAST::codegen() {
  Function *CalleeF = llvm_module->getFunction(static_cast<IdentifierAST*>(_name)->getName());
  if (!CalleeF) {
    LogErrorV("Unknown Function Referenced");
    return nullptr;
  }

  if (_argument_list) {
    if (CalleeF->arg_size() != _argument_list->getSize()) {
      LogErrorV("Incorrect number of arguments passed");
      return nullptr;
    }
  } else {
    if (CalleeF->arg_size() != 0) {
      LogErrorV("Incorrect number of arguments passed");
      return nullptr;
    }
  }

  std::vector<llvm::Value*> ArgsV;
  if (_argument_list) {
    for (auto &arg : _argument_list->getArgs()) {
      ArgsV.push_back(arg->codegen());
      if (!ArgsV.back()) {
        return nullptr;
      }
    }
  }

  return llvm_builder->CreateCall(CalleeF, ArgsV, "calltmp");
}

void RootAST::codegen()
{
  for(auto ed: _extern_units)
  {
    ed->codegen();
  }
}

void BlockItemListAST::codegen()
{
  nested_symbols.push_back({});

  for(auto bi: _items)
  {
    bi->codegen();
  }

  nested_symbols.pop_back();
}

Type* DeclSpecifiersAST::getLLVMType()
{
  PrimitiveTypeSpecAST* prim_spec = nullptr;
  for (auto ptr : _decl_specs)
  {
    prim_spec = dynamic_cast<PrimitiveTypeSpecAST*>(ptr);
    if (prim_spec)
      break;
  }

  if(!prim_spec)
  {
    LogErrorV("No primitive type specifier present");
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
    _func_declarator->codegen(_decl_specs);

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
  bool no_ret = false;
  Type *ret_type = _decl_specs->getLLVMType();
  Value *ret_alloca = nullptr;
  no_ret = !isReturnPresent();

  if(no_ret && !ret_type->isVoidTy())
  {
    ret_alloca = CreateEntryBlockAlloca(F, "dummy_ret", ret_type);
  }

  _compound_stmts->codegen();

  if(no_ret)
  {
    if(ret_alloca)
    {
      Value * ret_value = llvm_builder->CreateLoad(ret_type, ret_alloca, "retval");
      llvm_builder->CreateRet(ret_value);
    }
    else
      llvm_builder->CreateRetVoid();
  }
  verifyFunction(*F);
}

/*Value* InitializerAST::codegen() {
  if (_init_list) {
  return _init_list->codegen();
  } else if (_assignment_expression) {
  return _assignment_expression->codegen();
  } else {
  LogErrorV("No initializer");
  return nullptr;
  }
  }*/

void NormalDeclAST::codegen()
{
  _init_decl_list->codegen(_specs);
}

void InitDeclaratorListAST::codegen(DeclSpecifiersAST *specs)
{
  for (auto idecl: _init_declarators)
  {
    idecl->codegen(specs);
  }
}

Value *InitializerAST::codegen()
{
  return _assignment_expression->codegen();
}

Value *InitializerListAST::codegen()
{
  LogErrorV("InitDeclaratorListAST codegen not implemented yet");
  return nullptr;
}

void InitDeclaratorAST::codegen(DeclSpecifiersAST *specs)
{
  _direct_decl->codegen(specs);
  Value *val = nullptr; 

  if (_initializer)
  {
    if (nested_symbols.size() == 0)
    {
      // global initializer;
    }
    else{
      val = _initializer->codegen();
      if (!val)
      {
        LogErrorV("expression did not return a value");
        return;
      }
      auto A = nested_symbols.back()[_direct_decl->getName()];
      llvm_builder->CreateStore(val, (Value *) A);
    }
  }
}

void FunctionDeclaratorAST::codegen(DeclSpecifiersAST *specs)
{
  // incorporate ellipsis
  auto param_types = _paramlist->getParamTypes();
  auto param_names = _paramlist->getParamNames();

  Type *ret_ty = static_cast<IdDeclaratorAST*>(_identifier)->getLLVMType(specs);

  FunctionType* FT = FunctionType::get(ret_ty, param_types , false);
  Function* F = Function::Create(FT, Function::ExternalLinkage, _identifier->getName(), *llvm_module);

  unsigned int i = 0;
  for (auto &Arg: F->args())
  {
    Arg.setName(param_names[i++]);
  }
}

Type *IdDeclaratorAST::getLLVMType(DeclSpecifiersAST *specs)
{
  Type *ty = specs->getLLVMType();
  if (_pointer)
  {
    int numstars = _pointer->getSize();
    for(int i = 0; i < numstars; i++)
    {
      ty = PointerType::get(ty, 0); 
    }
  }
  return ty;
}

void IdDeclaratorAST::codegen(DeclSpecifiersAST *specs)
{
  if (nested_symbols.size() == 0){
    if(global_symbols.find(_name) != global_symbols.end())
    {
      LogErrorV("Variable already exists");
      return;
    }
  }   
  else if (nested_symbols.back().find(_name) != nested_symbols.back().end())
  {
    LogErrorV("Variable already exists");
    return;
  }

  Type *ty = getLLVMType(specs);

  if (nested_symbols.size() == 0)
  {
    cout << "global scope" << endl;
  }
  else
  {
    Function *F = llvm_builder->GetInsertBlock()->getParent();
    AllocaInst *A = CreateEntryBlockAlloca(F, _name, ty);  
    nested_symbols.back()[_name] = A;
  }
}
