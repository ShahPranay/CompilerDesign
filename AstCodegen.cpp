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

std::map<std::string, VarData> global_symbols;
std::vector<std::map<std::string, VarData>> nested_symbols;
std::map<std::string, FunctionTypeInfo*> function_types;

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

ExprRet* BooleanExprAST::codegen() {
  TypeInfo *ty = new TypeInfo();
  return new ExprRet(ty, ConstantInt::get(*llvm_context, APInt(1, _val ? 1 : 0)));
}

ExprRet* IntegerExprAST::codegen() {
  TypeInfo *ty = new TypeInfo();
  return new ExprRet(ty, ConstantInt::get(*llvm_context, APInt(32, _val, true)));
}

ExprRet* DoubleExprAST::codegen() {
  TypeInfo *ty = new TypeInfo();
  return new ExprRet(ty, ConstantFP::get(*llvm_context, APFloat(_val)));
}

VarData IdentifierAST::getVarData()
{
  VarData V;
  bool found = false;

  for (int i = nested_symbols.size() - 1; i >= 0; i--){
    if (nested_symbols[i].find(_name) != nested_symbols[i].end())
    {
      V = nested_symbols[i][_name];
      found = true;
      break;
    }
  }

  if (!found)
  {
    V = global_symbols[_name];
    found = true;
  }

  if (!found)
  {
    cout << _name << endl;
    LogErrorV("Unknown Identifier name");
  }

  return V;
}

ExprRet* StrLiteralAST::codegen()
{
  /* ArrayType *strtype = ArrayType::get(Type::getInt8Ty(*llvm_context), _str.size()); */
  /* GlobalVariable *globalvar = new GlobalVariable(*llvm_module, strtype, false, GlobalValue::ExternalLinkage, nullptr, ""); */
  /* globalvar->setInitializer(ConstantDataArray::getString(*llvm_context, _str, false)); */
  /* globalvar->setConstant(true); */

  /* globalvar->setAlignment(MaybeAlign(1)); */
  TypeInfo *ty = new TypeInfo();
  return new ExprRet(ty, llvm_builder->CreateGlobalString(_str, "str"));
}

ExprRet* IdentifierAST::codegen() 
{
  VarData V = getVarData();
  AllocaInst *A = V.allocainst;
  if (!A) {
    return nullptr;  // If allocation instruction not found, return nullptr
  }
  // copy typeinfo
  Value *val = llvm_builder->CreateLoad(A->getAllocatedType(), A, _name.c_str());
  return new ExprRet(V.type, val);
}

/*ExprRet* UnaryExprAST::codegen() {
  llvm::Value* val = _expr->codegen();
  
  if (!val) 
  {
    return nullptr;
  }

  if (_op == "&") 
  {
    return nullptr;
    //Todo
  } 
  else if (_op == "*") 
  {
    if (!val->getType()->isPointerTy()) {
      LogErrorV("Cannot dereference non-pointer type");
      return nullptr;
    }
    Type *deref_ty = val->getType()->getNonOpaquePointerElementType();
    return llvm_builder->CreateLoad(deref_ty, val, "deref_val");
  } 
  else if (_op == "+") 
  {
    return val;
  }
  else if (_op == "-")
  {
    return llvm_builder->CreateNeg(val);
  }
  else if (_op == "~") 
  {
    return llvm_builder->CreateNot(val);
  }
  else if (_op == "!") 
  {
    llvm::Type *boolTy = llvm::Type::getInt1Ty(*llvm_context);
    return llvm_builder->CreateICmpEQ(val, llvm::Constant::getNullValue(boolTy));
  }
  else if (_op == "++") 
  {
    if (!val->getType()->isIntegerTy()) {
        LogErrorV("Cannot decrement non-integer type");
        return nullptr;
    }
    Value *one = llvm::ConstantInt::get(val->getType(), 1);
    return llvm_builder->CreateAdd(val, one, "increment");
  }
  else if (_op == "--") 
  {
    if (!val->getType()->isIntegerTy()) {
        LogErrorV("Cannot decrement non-integer type");
        return nullptr;
    }
    Value *one = llvm::ConstantInt::get(val->getType(), 1);
    return llvm_builder->CreateSub(val, one, "decrement");
  }
  else 
  {
    cout << _op << endl;
    LogErrorV("Invalid unary operator");
    return nullptr;
  }
}*/

ExprRet* BinaryExprAST::codegen() 
{
  ExprRet *lret, *rret;
  Value *L, *R, *retvalue = nullptr;

  if (op == "=")
  {
    IdentifierAST *LHSE = static_cast<IdentifierAST*>(left);
    if (!LHSE){
      LogErrorV("destination of \'=\' must be a variable.");
      return nullptr;
    }

    rret = right->codegen();
    R = rret->getValue();
    if (!R)
      return nullptr;

    VarData lvalueData = LHSE->getVarData();
    Value *VarValue = lvalueData.allocainst; 
    if(!VarValue)
    {
      LogErrorV("Unknown variable name.");
      return nullptr;
    }

    if(!lvalueData.type->iscompatible(rret->getType()))
    {
      LogErrorV("Types not compatible");
      return nullptr;
    }

    llvm_builder->CreateStore(R, VarValue);
    return rret;
  }

  lret = left->codegen();
  rret = right->codegen();

  // typechecking

  L = lret->getValue();
  R = rret->getValue();

  if (!L || !R)
    return nullptr;

  if(op == "||")
  {
    if (L->getType()->isIntegerTy(1) && R->getType()->isIntegerTy(1))
      retvalue = llvm_builder->CreateOr(L, R, "cmptmp"); 
  }
  else if(op == "&&")
  {
    if (L->getType()->isIntegerTy(1) && R->getType()->isIntegerTy(1))
      retvalue = llvm_builder->CreateAnd(L, R, "cmptmp"); 
  }
  else if (L->getType()->isIntegerTy(32) && R->getType()->isIntegerTy(32)) {
    if (op == "+")
      retvalue = llvm_builder->CreateAdd(L, R, "addtmp");
    else if (op == "-")
      retvalue = llvm_builder->CreateSub(L, R, "subtmp");
    else if (op == "*")
      retvalue = llvm_builder->CreateMul(L, R, "multmp");
    else if (op == "/")
      retvalue = llvm_builder->CreateSDiv(L, R, "divtmp");
    else if (op == "%")
      retvalue = llvm_builder->CreateSRem(L, R, "modtmp");
    else if (op == "<")
      retvalue = llvm_builder->CreateICmpSLT(L, R, "cmptmp");
    else if (op == ">")
      retvalue = llvm_builder->CreateICmpSGT(L, R, "cmptmp");
    else if (op == "<=")
      retvalue = llvm_builder->CreateICmpSLE(L, R, "cmptmp");
    else if (op == ">=")
      retvalue = llvm_builder->CreateICmpSGE(L, R, "cmptmp");
    else if (op == "==")
      retvalue = llvm_builder->CreateICmpEQ(L, R, "cmptmp");
    else if (op == "!=")
      retvalue = llvm_builder->CreateICmpNE(L, R, "cmptmp");
    else if (op == "&")
      retvalue = llvm_builder->CreateAnd(L, R, "andtmp");
    else if (op == "|")
      retvalue = llvm_builder->CreateOr(L, R, "ortmp");
    else if (op == "^")
      retvalue = llvm_builder->CreateXor(L, R, "xortmp");
    else if (op == "<<")
      retvalue = llvm_builder->CreateShl(L, R, "shltmp");
    else if (op == ">>")
      retvalue = llvm_builder->CreateAShr(L, R, "ashrtmp");
    else
    {
      LogErrorV("Invalid binary operator");
      return nullptr;
    }
  } else if (L->getType()->isDoubleTy() && R->getType()->isDoubleTy()) {
    if (op == "+")
      retvalue = llvm_builder->CreateFAdd(L, R, "addtmp");
    else if (op == "-")
      retvalue = llvm_builder->CreateFSub(L, R, "subtmp");
    else if (op == "*")
      retvalue = llvm_builder->CreateFMul(L, R, "multmp");
    else if (op == "/")
      retvalue = llvm_builder->CreateFDiv(L, R, "divtmp");
    else if (op == "<")
    {
      L = llvm_builder->CreateFCmpULT(L, R, "cmptmp");
      retvalue = llvm_builder->CreateUIToFP(L, Type::getDoubleTy(*llvm_context), "booltmp");
    }
    else if (op == ">")
    {
      L = llvm_builder->CreateFCmpUGT(L, R, "cmptmp");
      retvalue = llvm_builder->CreateUIToFP(L, Type::getDoubleTy(*llvm_context), "booltmp");
    }
    else if (op == "<=")
    {
      L = llvm_builder->CreateFCmpULE(L, R, "cmptmp");
      retvalue = llvm_builder->CreateUIToFP(L, Type::getDoubleTy(*llvm_context), "booltmp");
    }
    else if (op == ">=")
    {
      L = llvm_builder->CreateFCmpUGE(L, R, "cmptmp");
      retvalue = llvm_builder->CreateUIToFP(L, Type::getDoubleTy(*llvm_context), "booltmp");
    }
    else if (op == "==")
    {
      L = llvm_builder->CreateFCmpUEQ(L, R, "cmptmp");
      retvalue = llvm_builder->CreateUIToFP(L, Type::getDoubleTy(*llvm_context), "booltmp");
    }
    else if (op == "!=")
    {
      L = llvm_builder->CreateFCmpUNE(L, R, "cmptmp");
      retvalue = llvm_builder->CreateUIToFP(L, Type::getDoubleTy(*llvm_context), "booltmp");
    }
    else
    {
      LogErrorV("Invalid binary operator");
      return nullptr;
    }
  }
  else
  {
    LogErrorV("Invalid binary operator");
    return nullptr;
  }

  // TODO: consider both lret and rret.
  return new ExprRet(lret->getType(), retvalue);
}

void ExprStmtAST::codegen()
{
  _expression->codegen();
}

void IfElseStmtAST::codegen()
{
  ExprRet *CondRet = _expression->codegen();
  Value *CondV = CondRet->getValue();
  if (!CondV) {
    LogErrorV("Failed to generate code for if condition");
    return;
  }

  if (!CondV->getType()->isIntegerTy(1)) {
    LogErrorV("If condition must be of type boolean");
    return;
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
  {
    _else_statement->codegen();
  }
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

  return;
}

void ReturnStmtAST::codegen()
{
  if (_expr) {
    ExprRet *Ret = _expr->codegen();
    Value *RetVal = Ret->getValue();
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

  ExprRet *CondRet = _expression->codegen();
  Value *ConditionValue = CondRet->getValue();

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

  return;
}

ExprRet* FunctionCallAST::codegen() {
  std::string funcname = static_cast<IdentifierAST*>(_name)->getName();
  Function *CalleeF = llvm_module->getFunction(funcname);
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
      ExprRet *tmpret = arg->codegen();
      // check type
      ArgsV.push_back(tmpret->getValue());

      if (!ArgsV.back()) {
        return nullptr;
      }
    }
  }

  Value *resultVal = llvm_builder->CreateCall(CalleeF, ArgsV, "calltmp");
  FunctionTypeInfo *fty = function_types[funcname];
  return new ExprRet(fty->getReturnTypeInfo(), resultVal);
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

BaseType DeclSpecifiersAST::getBaseType()
{
  PrimitiveTypeSpecAST* prim_spec = nullptr;
  BaseType bt = constant_ty;
  Qualifier ql = no_qual;

  for (auto ptr: _decl_specs)
  {
    prim_spec = dynamic_cast<PrimitiveTypeSpecAST*>(ptr);
    if (prim_spec)
      break;
  }

  if(!prim_spec)
  {
    LogErrorV("Not a primitive Type Specifier");
    return bt;
  }

  std::string type_name = prim_spec->getName();

  if(type_name == "int")
    bt = BaseType::int_ty;
  else if(type_name == "char")
    bt = BaseType::char_ty;
  else if(type_name == "double")
    bt = BaseType::double_ty;
  else if(type_name == "void")
    bt = BaseType::void_ty;
  else 
  {
    LogErrorV("Invalid Type");
    return bt;
  }

  return bt;
}

Type* TypeInfo::getLLVMType() 
{
  if (!_ptrinfo.empty()) {
    return PointerType::get(*llvm_context, 0);
  }

  Type* llvm_type = nullptr;

  if (_basetype == BaseType::constant_ty ) {
    llvm_type = Type::getInt32Ty(*llvm_context);
  } else if (_basetype == BaseType::void_ty ) {
    llvm_type = Type::getVoidTy(*llvm_context);
  } else if (_basetype == BaseType::char_ty ) {
    llvm_type = Type::getInt8Ty(*llvm_context);
  } else if (_basetype == BaseType::short_ty) {
    llvm_type = Type::getInt16Ty(*llvm_context);
  } else if (_basetype == BaseType::int_ty ) {
    llvm_type = Type::getInt32Ty(*llvm_context);
  } else if (_basetype == BaseType::long_ty ) {
    llvm_type = Type::getInt64Ty(*llvm_context);
  } else if (_basetype == BaseType::float_ty) {
    llvm_type = Type::getFloatTy(*llvm_context);
  } else if (_basetype == BaseType::double_ty ) {
    llvm_type = Type::getDoubleTy(*llvm_context);
  } else if (_basetype == BaseType::bool_ty ) {
    llvm_type = Type::getInt1Ty(*llvm_context);
  }

  return llvm_type;
}

std::vector<Qualifier> PointerAST::getQualVec()
{
  std::vector<Qualifier> ret;
  for (auto ptr: _pointer_list)
  {
    if (ptr)
      ret.push_back(Qualifier::const_qual);
    else ret.push_back(Qualifier::no_qual);
  }
  return ret;
}

TypeInfo::TypeInfo(DeclSpecifiersAST *specs, DirectDeclaratorAST *decl)
{
  _basetype = specs->getBaseType();
  _basequalifier = Qualifier::no_qual;
  PointerAST *ptr = decl->getPointer();
  if (ptr)
    _ptrinfo = ptr->getQualVec();
}

bool TypeInfo::iscompatible(TypeInfo *other)
{
  return true;
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

std::vector<Type*> FunctionTypeInfo::getLLVMParamTypes()
{
  std::vector<Type*> ret;
  for (auto ty: _paramTypes)
  {
    ret.push_back(ty->getLLVMType());
  }
  return ret;
}

void FunctionDefinitionAST::codegen()
{
  std::string funcname = _func_declarator->getName();
  Function *F = llvm_module->getFunction(funcname);

  if(!F)
    _func_declarator->codegen(_decl_specs);

  F = llvm_module->getFunction(funcname);

  if(!F)
    return;

  if(!F->empty()){
    LogErrorV("Cannot be redefined");
    return;
  }


  FunctionTypeInfo *fty = function_types[funcname];
  auto param_typeinfos = fty->getParamTypeInfos();

  BasicBlock *BB = BasicBlock::Create(*llvm_context, "entry", F);
  llvm_builder->SetInsertPoint(BB);

  nested_symbols.clear();
  nested_symbols.push_back({});
  int id = 0;
  for (auto &arg: F->args())
  {
    AllocaInst *Alloca = CreateEntryBlockAlloca(F, arg.getName(), arg.getType());
    llvm_builder->CreateStore(&arg, Alloca);
    nested_symbols[0][std::string(arg.getName())] = VarData(param_typeinfos[id++], Alloca);
  }

  // body codgen
  bool no_ret = false;
  Type *ret_type = fty->getReturnTypeInfo()->getLLVMType();
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
      Value *ret_value = llvm_builder->CreateLoad(ret_type, ret_alloca, "retval");
      llvm_builder->CreateRet(ret_value);
    }
    else
      llvm_builder->CreateRetVoid();
  }

  nested_symbols.pop_back();

  verifyFunction(*F);
}

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

ExprRet *InitializerAST::codegen()
{
  return _assignment_expression->codegen();
}

ExprRet *InitializerListAST::codegen()
{
  LogErrorV("InitDeclaratorListAST codegen not implemented yet");
  return nullptr;
}

void InitDeclaratorAST::codegen(DeclSpecifiersAST *specs)
{
  _direct_decl->codegen(specs);
  if (!_initializer)
    return;

  if ( nested_symbols.size() == 0 )
  {
    // global scope
  }
  else
  {
    Value *val = nullptr; 
    ExprRet *ret = _initializer->codegen();
    val = ret->getValue();
    if (!val)
    {
      LogErrorV("expression did not return a value");
      return;
    }

    std::string varname = _direct_decl->getName();
    VarData var = nested_symbols.back()[varname];

    llvm_builder->CreateStore(val, (Value *) var.allocainst);
  }
}

std::vector<TypeInfo *> ParamListAST::getParamTypeInfos()
{
  std::vector<TypeInfo *> ret;
  for (auto param : _params)
    ret.push_back(param->getTypeInfo());

  // TODO: handle ellipsis 
  return ret;
}

void FunctionDeclaratorAST::codegen(DeclSpecifiersAST *specs)
{
  // incorporate ellipsis
  std::string funcname = _identifier->getName();
  TypeInfo *RetTypeInfo = new TypeInfo(specs, _identifier);
  Type *retType = RetTypeInfo->getLLVMType();


  auto param_typeinfos = _paramlist->getParamTypeInfos();
  auto param_names = _paramlist->getParamNames();

  FunctionTypeInfo *fty = new FunctionTypeInfo(RetTypeInfo, param_typeinfos);

  function_types[funcname] = fty;

  auto param_llvmtypes = fty->getLLVMParamTypes();

  FunctionType* FT = FunctionType::get(retType, param_llvmtypes , false);

  Function* F = Function::Create(FT, Function::ExternalLinkage, funcname, *llvm_module);

  unsigned int i = 0;
  for (auto &Arg: F->args())
  {
    Arg.setName(param_names[i++]);
  }
}

void IdDeclaratorAST::codegen(DeclSpecifiersAST *specs)
{
  if(nested_symbols.size() == 0)
  {
    // global variable
  }
  else
  {
    if (nested_symbols.back().find(_name) != nested_symbols.back().end())
    {
      LogErrorV("Variable already exists");
      return;
    }
    TypeInfo *ty = new TypeInfo(specs, this);
    Function *F = llvm_builder->GetInsertBlock()->getParent();
    AllocaInst *A = CreateEntryBlockAlloca(F, _name, ty->getLLVMType());  

    nested_symbols.back()[_name] = {ty, A};
  }
  //cout << "Declared variable: " << _name << endl;
}
