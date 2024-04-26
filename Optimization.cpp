#include "AstNodes.h"

#include <stack>
#include <map>
#include <iostream>

using std::cout, std::endl;

ExprAST* BinaryExprAST::constantFolding(ExprAST* _expr) {
    ExprAST* L = constantFolding(left);
    ExprAST* R = constantFolding(right);

    if (dynamic_cast<IntegerExprAST*>(L))
}