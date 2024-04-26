#include "AstNodes.h"

#include <stack>
#include <map>
#include <iostream>

using std::cout, std::endl;

ExprAST* BinaryExprAST::constantFolding() 
{
    ExprAST* L = left->constantFolding();
    if (left != L)
    {
      delete left;
      left = L;
    }

    ExprAST* R = right->constantFolding();
    if (right != R)
    {
      delete right;
      right = R;
    }

    IntegerExprAST *Lint = dynamic_cast<IntegerExprAST *>(left), 
                   *Rint = dynamic_cast<IntegerExprAST *>(right);

    if (!Lint || !Rint)
      return this;
    
    if (op == "+")
    {
      return new IntegerExprAST(left->getVal() + right->getVal());
    }
}
