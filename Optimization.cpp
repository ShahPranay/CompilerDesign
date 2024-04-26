#include "AstNodes.h"

#include <stack>
#include <map>
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
        if (_op == "++") {
            return new IntegerExprAST(intexpr->getVal() + 1);
        }
        if (_op == "--") {
            return new IntegerExprAST(intexpr->getVal() - 1);
        }
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
        /*if (op == "<")
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
        return llvm_builder->CreateICmpNE(L, R, "cmptmp");*/
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
        /*if (op == "<") {
            L = llvm_builder->CreateFCmpULT(L, R, "cmptmp");
            return llvm_builder->CreateUIToFP(L, Type::getDoubleTy(*llvm_context), "booltmp");
        }
        if (op == ">") {
            L = llvm_builder->CreateFCmpUGT(L, R, "cmptmp");
            return llvm_builder->CreateUIToFP(L, Type::getDoubleTy(*llvm_context), "booltmp");
        }
        if (op == "<=") {
            L = llvm_builder->CreateFCmpULE(L, R, "cmptmp");
            return llvm_builder->CreateUIToFP(L, Type::getDoubleTy(*llvm_context), "booltmp");
        }
        if (op == ">=") {
            L = llvm_builder->CreateFCmpUGE(L, R, "cmptmp");
            return llvm_builder->CreateUIToFP(L, Type::getDoubleTy(*llvm_context), "booltmp");
        }
        if (op == "==") {
            L = llvm_builder->CreateFCmpUEQ(L, R, "cmptmp");
            return llvm_builder->CreateUIToFP(L, Type::getDoubleTy(*llvm_context), "booltmp");
        }
        if (op == "!=") {
            L = llvm_builder->CreateFCmpUNE(L, R, "cmptmp");
            return llvm_builder->CreateUIToFP(L, Type::getDoubleTy(*llvm_context), "booltmp");
        }*/
    }

    return this;
}

StmtAST* ExprStmtAST::constantFolding() {
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
    StmtAST* else_statement =_else_statement->constantFolding();
    if (else_statement != _else_statement) {
        delete _else_statement;
        _else_statement = else_statement;
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
    for(int i = 0;i<_items.size(); i++) {
        BlockItemAST* item = _items[i]->constantFolding();
        if (item != _items[i]) {
            delete _items[i];
            _items[i] = item;
        }
    }
    return this;
}

BlockItemAST* FunctionDefinitionAST::constantFolding() {
    _compound_stmts->constantFolding();
    return this;
}

/*void ArrayDesignatorAST::constantFolding() {
    ExprAST* expr = _expr->constantFolding();
    if (expr != _expr) {
        delete _expr;
        _expr = expr;
    }
}

void DesignatorListAST::constantFolding() {
    for(auto designator : _designator_list) {
        designator->constantFolding();
    }
}*/

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
    _initializer->constantFolding();
}

void InitDeclaratorListAST::constantFolding() {
    for(auto init_declarator : _init_declarators) {
        init_declarator->constantFolding();
    }
}

BlockItemAST* NormalDeclAST::constantFolding() {
    _init_decl_list->constantFolding();
    return this;
}


