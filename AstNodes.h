#include <string>
#include <vector>
#include <memory>


//Base class for all nodes
class NodeAST {
  public:
    virtual ~NodeAST() = default;
};

//Base class for all exressions
class ExprAST : public NodeAST {
};

// Base class for all statements
class StmtAST : public NodeAST {

};

class IntegerExprAST : public ExprAST {
  int val;

  public:
  IntegerExprAST(int Val) : val(Val) {}
};

class DoubleExprAST : public ExprAST {
  double val;

  public:
  DoubleExprAST(double Val) : val(Val) {}
};

class IdentifierAST : public ExprAST {
  std::string _name;

  public:
  IdentifierAST(const std::string& name) : _name( name ) {  }
};

// Binary expression AST node
class BinaryExprAST : public ExprAST {
  std::string op;
  std::unique_ptr<ExprAST> left, right;

  public:
  BinaryExprAST(std::string op, std::unique_ptr<ExprAST> LHS, std::unique_ptr<ExprAST> RHS)
    : op(op), left(std::move(LHS)), right(std::move(RHS)) {}
};

class ExprStmtAST : public StmtAST {
  std::unique_ptr<ExprAST> _expression;

  public:
  ExprStmtAST(std::unique_ptr<ExprAST>& expression) : _expression(std::move(expression)) {  }
  ExprStmtAST() : _expression(std::make_unique<ExprAST>(nullptr)) {  }
};

class BlockItemListAST : public StmtAST {
  std::vector<std::unique_ptr<StmtAST>> _statements;

  public:
  BlockItemListAST() {}
  insertStatement(std::unique_ptr<StmtAST>& stmt) {
    _statements.push_back(std::move(stmt));
  }
};

class DirectDeclaratorAST : public NodeAST { 
  PointerAst _pointer; // to write
};

class FunctionDeclaratorAST : public DirectDeclaratorAST {
  std::unique_ptr<DirectDeclaratorAST> _identifier;
  std::unique_ptr<ParamListAST> _paramlist;
};

class IdDeclaratorAST : public NodeAST {
  std::string _name;

  public:
  IdDeclaratorAST(const std::string& name) : _name(name) {  }
};

class ParamListAST : public NodeAST {
  bool _isEllipsis;
  std::vector<ParamDeclAST> _params;

  public:
  ParamDeclAST(bool isellipsis) : _isEllipsis(isellipsis) {  }
  insertParam(std::unique_ptr<ParamDeclAST>& param) 
  {
    _params.push_back(std::move(param));
  }
};

class ParamDeclAST : public NodeAST {
  std::unique_ptr<DeclSpecifierAST> _specs;
  std::unique_ptr<DirectDeclaratorAST> _decl;     
};

class DeclarationAST : public NodeAST {

};

class FunctionDefinitionAST : public  NodeAST {
  
};

class DeclSpecifierAST : public NodeAST {

};
