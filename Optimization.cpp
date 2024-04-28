#include "AstNodes.h"

#include <stack>
#include <map>
#include <set>
#include <string>
#include <iostream>

using std::cout, std::endl;

void RootAST::constantFolding()
{
  for(auto ed: _extern_units)
  {
    ed->constantFolding();
  }
}

ExprAST* UnaryExprAST::constantFolding() {
  ExprAST* expr = _expr->constantFolding();
  if (expr != _expr) {
    delete _expr;
    _expr = expr;
  }

  IntegerExprAST* intexpr = dynamic_cast<IntegerExprAST *>(_expr);

  if (intexpr) {
    if (_op == "~")
      return new IntegerExprAST(~intexpr->getVal());
    if (_op == "!")
      return new IntegerExprAST(!intexpr->getVal());
    if (_op == "+")
      return new IntegerExprAST(+intexpr->getVal());
    if (_op == "-")
      return new IntegerExprAST(-intexpr->getVal());
  }

  return this;
}

ExprAST* BinaryExprAST::constantFolding() {
  ExprAST* L = left->constantFolding();
  if (left != L) {
    delete left;
    left = L;
  }

  ExprAST* R = right->constantFolding();
  if (right != R) {
    delete right;
    right = R;
  }

  IntegerExprAST *Lint = dynamic_cast<IntegerExprAST *>(left), 
                 *Rint = dynamic_cast<IntegerExprAST *>(right);

  if (Lint && Rint) {
    if (op == "+")
      return new IntegerExprAST(Lint->getVal() + Rint->getVal());
    if (op == "-")
      return new IntegerExprAST(Lint->getVal() - Rint->getVal());
    if (op == "*")
      return new IntegerExprAST(Lint->getVal() * Rint->getVal());
    if (op == "/")
      return new IntegerExprAST(Lint->getVal() / Rint->getVal());
    if (op == "%")
      return new IntegerExprAST(Lint->getVal() % Rint->getVal());
    if (op == "<")
      return new BooleanExprAST(Lint->getVal() < Rint->getVal());
    if (op == ">")
      return new BooleanExprAST(Lint->getVal() > Rint->getVal());
    if (op == "<=")
      return new BooleanExprAST(Lint->getVal() <= Rint->getVal());
    if (op == ">=")
      return new BooleanExprAST(Lint->getVal() >= Rint->getVal());
    if (op == "==")
      return new BooleanExprAST(Lint->getVal() == Rint->getVal());
    if (op == "!=")
      return new BooleanExprAST(Lint->getVal() != Rint->getVal());
    if (op == "&")
      return new IntegerExprAST(Lint->getVal() & Rint->getVal());
    if (op == "|")
      return new IntegerExprAST(Lint->getVal() | Rint->getVal());
    if (op == "^")
      return new IntegerExprAST(Lint->getVal() ^ Rint->getVal());
    if (op == "<<")
      return new IntegerExprAST(Lint->getVal() << Rint->getVal());
    if (op == ">>")
      return new IntegerExprAST(Lint->getVal() >> Rint->getVal());
  }

  DoubleExprAST *Ldouble = dynamic_cast<DoubleExprAST *>(left), 
                *Rdouble = dynamic_cast<DoubleExprAST *>(right);

  if (Ldouble && Rdouble) {
    if (op == "+")
      return new DoubleExprAST(Ldouble->getVal() + Rdouble->getVal());
    if (op == "-")
      return new DoubleExprAST(Ldouble->getVal() - Rdouble->getVal());
    if (op == "*")
      return new DoubleExprAST(Ldouble->getVal() * Rdouble->getVal());
    if (op == "/")
      return new DoubleExprAST(Ldouble->getVal() / Rdouble->getVal());
    if (op == "<")
      return new BooleanExprAST(Ldouble->getVal() < Rdouble->getVal());
    if (op == ">")
      return new BooleanExprAST(Ldouble->getVal() > Rdouble->getVal());
    if (op == "<=")
      return new BooleanExprAST(Ldouble->getVal() <= Rdouble->getVal());
    if (op == ">=")
      return new BooleanExprAST(Ldouble->getVal() >= Rdouble->getVal());
    if (op == "==")
      return new BooleanExprAST(Ldouble->getVal() == Rdouble->getVal());
    if (op == "!=")
      return new BooleanExprAST(Ldouble->getVal() != Rdouble->getVal());
  }

  BooleanExprAST *LBool = dynamic_cast<BooleanExprAST *>(left),
                 *RBool = dynamic_cast<BooleanExprAST *>(right);

  if (LBool && RBool) {
    if (op == "||")
      return new BooleanExprAST(LBool->getVal() || RBool->getVal());
    if (op == "&&")
      return new BooleanExprAST(LBool->getVal() && RBool->getVal());
    if (op == "==")
      return new BooleanExprAST(LBool->getVal() == RBool->getVal());
    if (op == "!=")
      return new BooleanExprAST(LBool->getVal() != RBool->getVal());
  }

  return this;
}

StmtAST* ExprStmtAST::constantFolding() {
  if (!_expression)
    return nullptr; // empty statement
                    //
  ExprAST* expression = _expression->constantFolding();
  if (expression != _expression) {
    delete _expression;
    _expression = expression;
  }
  return this;
}

StmtAST* IfElseStmtAST::constantFolding() {
  ExprAST* expression = _expression->constantFolding();
  if (expression != _expression) {
    delete _expression;
    _expression = expression;
  }
  StmtAST* then_statement = _then_statement->constantFolding();
  if (then_statement != _then_statement) {
    delete _then_statement;
    _then_statement = then_statement;
  }
  if (_else_statement)
  {
    StmtAST* else_statement =_else_statement->constantFolding();
    if (else_statement != _else_statement) {
      delete _else_statement;
      _else_statement = else_statement;
    }
  }

  // both then and else can be null now

  IntegerExprAST *expint = dynamic_cast<IntegerExprAST *>(_expression); 
  if (expint)
  {
    int val = expint->getVal();
    if (val == 0)
      return _else_statement;
    else return _then_statement;
  }

  BooleanExprAST *expbool = dynamic_cast<BooleanExprAST *>(_expression);
  if(expbool)
  {
    if (expbool->getVal())
      return _then_statement;
    else return _else_statement;
  }

  if (!_then_statement)
  {
    if (!_else_statement)
    {
      return nullptr;
    }
    else 
    {
      _expression = new UnaryExprAST("!", _expression);
      _then_statement = _else_statement;
      _else_statement = nullptr;
    }
  }

  return this;
}

StmtAST* WhileStmtAST::constantFolding() {

  ExprAST* expression = _expression->constantFolding();
  if (expression != _expression) {
    delete _expression;
    _expression = expression;
  }
  StmtAST* statement = _statement->constantFolding();
  if (statement != _statement) {
    delete _statement;
    _statement = statement;
  }

  if (!_statement)
    return this;

  IntegerExprAST *expint = dynamic_cast<IntegerExprAST *>(_expression); 
  if (expint)
  {
    int val = expint->getVal();
    if (val == 0)
      return nullptr;
  }

  BooleanExprAST *expbool = dynamic_cast<BooleanExprAST *>(_expression);
  if(expbool)
  {
    if (!expbool->getVal())
      return nullptr;
  }

  return this;
}

StmtAST* ReturnStmtAST::constantFolding() {
  ExprAST* expr = _expr->constantFolding();
  if (expr != _expr) {
    delete _expr;
    _expr = expr;
  }
  return this;
}

void ArgListAST::constantFolding() {
  for(int i = 0; i < _arg_list.size(); i++) {
    ExprAST* arg = _arg_list[i]->constantFolding();
    if (arg != _arg_list[i]) {
      delete _arg_list[i];
      _arg_list[i] = arg;
    } 
  }
}

ExprAST* FunctionCallAST::constantFolding() {
  _argument_list->constantFolding();
  return this;
}

StmtAST* BlockItemListAST::constantFolding() {
  if (isEmpty())
    return nullptr;

  bool isanynull = false;
  for(int i = 0;i<_items.size(); i++) 
  {
    BlockItemAST* item = _items[i]->constantFolding();
    if (!item)
    {
      isanynull = true;
      _items[i] = nullptr;
    }
    else if (item != _items[i]) {
      delete _items[i];
      _items[i] = item;
    }
  }

  if (isanynull)
  {
    BlockItemListAST *ret = new BlockItemListAST();
    for (auto item: _items)
    {
      if (item)
        ret->insertBlockItem(item);
    }

    _items.clear();

    if (ret->isEmpty())
      return nullptr;
    else
      return ret;
  }

  return this;
}

void FunctionDefinitionAST::constantFolding() {
  auto bil = _compound_stmts->constantFolding();
  if(!bil)
    _compound_stmts = nullptr;
  if (bil && bil != _compound_stmts)
    _compound_stmts = static_cast<BlockItemListAST *>(bil);
}

void InitializerAST::constantFolding() {
  //_designation->constantFolding();
  ExprAST* assgn = _assignment_expression->constantFolding();
  if (assgn != _assignment_expression) {
    delete _assignment_expression;
    _assignment_expression = assgn;
  }
}

void InitializerListAST::constantFolding() {
  for(auto initializer : _initializer_list) {
    initializer->constantFolding();
  }
}

void InitDeclaratorAST::constantFolding() {
  if (_initializer)
    _initializer->constantFolding();
}

void InitDeclaratorListAST::constantFolding() {
  for(auto init_declarator : _init_declarators) 
  {
    init_declarator->constantFolding();
  }
}

BlockItemAST *NormalDeclarationAST::constantFolding() {
  _init_decl_list->constantFolding();
  return this;
}


void RootAST::localDeadCodeElim()
{
  for(auto edcl : _extern_units)
    edcl->localDeadCodeElim();
}

void FunctionDefinitionAST::localDeadCodeElim()
{
  std::set<std::string> deadVariables;
  if (_compound_stmts)
    _compound_stmts->localDeadCodeElim(deadVariables);
}

bool BlockItemListAST::localDeadCodeElim(std::set<std::string>& deadVariables)
{
  deadVariables.clear();

  std::vector<BlockItemAST *> tmpvec;
  for(int i = _items.size() - 1; i >= 0; i--)
  {
    if (!_items[i]->localDeadCodeElim(deadVariables))
    {
      tmpvec.push_back(_items[i]);
    }
  }


  deadVariables.clear();

  if (_items.size() == tmpvec.size())
    return false;

  _items.clear();
  for (int i = tmpvec.size() - 1; i >= 0; i--)
  {
    _items.push_back(tmpvec[i]);
  }


  return false;
}

bool ExprStmtAST::localDeadCodeElim(std::set<std::string>& deadVariables)
{
  bool present_before = false, present_after = false;
  std::string name;
  IdentifierAST *idtest;

  if (!_expression)
    return false;

  BinaryExprAST *bintest = dynamic_cast<BinaryExprAST *>(_expression); 
  if (!bintest || bintest->getOp() != "=")
    goto notelim;

  idtest = dynamic_cast<IdentifierAST *>(bintest->getLeft());
  if (!idtest)
    goto notelim;
    
  name = idtest->getName();

  present_before = (deadVariables.find(name) != deadVariables.end());    


  if (present_before)
    return true;

  deadVariables.insert(name);
  bintest->getRight()->livenessAnalysis(deadVariables);
  present_after = (deadVariables.find(name) != deadVariables.end());    


  return false;

notelim:
  _expression->livenessAnalysis(deadVariables);
  return false;
}

bool IfElseStmtAST::localDeadCodeElim(std::set<std::string>& deadVariables)
{
  deadVariables.clear();
  if (_else_statement)
    _else_statement->localDeadCodeElim(deadVariables);

  deadVariables.clear();

  _then_statement->localDeadCodeElim(deadVariables);
  deadVariables.clear();

  return false;
}

bool WhileStmtAST::localDeadCodeElim(std::set<std::string>& deadVariables)
{
  deadVariables.clear();
  _statement->localDeadCodeElim(deadVariables);
  deadVariables.clear();

  return false;
}

bool ReturnStmtAST::localDeadCodeElim(std::set<std::string>& deadVariables)
{
  _expr->livenessAnalysis(deadVariables);
  return false;
}

bool NormalDeclarationAST::localDeadCodeElim(std::set<std::string>& deadVariables)
{
  if (_init_decl_list)
    _init_decl_list->livenessAnalysis(deadVariables);
  return false;
}

void InitDeclaratorListAST::livenessAnalysis(std::set<std::string>& deadVariables)
{
  for (auto ptr: _init_declarators)
  {
    if (ptr)
      ptr->livenessAnalysis(deadVariables);
  }
}

void InitDeclaratorAST::livenessAnalysis(std::set<std::string>& deadVariables)
{
  if (_initializer)
    _initializer->livenessAnalysis(deadVariables); 
}

void InitializerAST::livenessAnalysis(std::set<std::string>& deadVariables)
{
  _assignment_expression->livenessAnalysis(deadVariables);  
}

void BinaryExprAST::livenessAnalysis(std::set<std::string>& deadVariables)
{
  if (op != "=")
    left->livenessAnalysis(deadVariables); 
  right->livenessAnalysis(deadVariables);
}

void FunctionCallAST::livenessAnalysis(std::set<std::string>& deadVariables)
{
  for(auto ptr: getArgs())
  {
    ptr->livenessAnalysis(deadVariables);
  }
}

void IdentifierAST::livenessAnalysis(std::set<std::string>& deadVariables)
{
  deadVariables.erase(_name);
}

void UnaryExprAST::livenessAnalysis(std::set<std::string>& deadVariables)
{
  if(_expr)
    _expr->livenessAnalysis(deadVariables);
}
