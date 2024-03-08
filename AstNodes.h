#include <string>
#include <vector>
#include <memory>

enum class NodeType {
  Expr,
  IntegerExpr,
  DoubleExpr,
  Identifier,
  BinaryExpr
};

//Base class for all nodes
class NodeAST {
  public:
    virtual ~NodeAST() = default;
    virtual NodeType getType() const = 0;
};

//Base class for all exressions
class ExprAST : public NodeAST {
  public:
    NodeType getType() const override { return NodeType::Expr; }
};

// Base class for all statements
class StmtAST : public NodeAST {

};

class IntegerExprAST : public ExprAST {
  int val;

  public:
  IntegerExprAST(int Val) : val(Val) {}

  int getVal() const { return val; }
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
  BinaryExprAST(const std::string& op, std::unique_ptr<ExprAST> LHS, std::unique_ptr<ExprAST> RHS)
    : op(op), left(std::move(LHS)), right(std::move(RHS)) {}

  const ExprAST* getLeft() const { return left.get(); }
  const ExprAST* getRight() const { return right.get(); }
  std::string getOp() const { return op; }
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
  void insertStatement(std::unique_ptr<StmtAST>& stmt) {
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
  std::vector<std::unique_ptr<ParamDeclAST>> _params;

  public:
  ParamListAST(bool isellipsis) : _isEllipsis(isellipsis) {  }
  void insertParam(std::unique_ptr<ParamDeclAST>& param) {
    _params.push_back(std::move(param));
  }
  void updateEllipsis(bool isellipsis) {
    _isEllipsis = isellipsis;
  }
};

class ParamDeclAST : public NodeAST {
  std::unique_ptr<DeclSpecifierAST> _specs;
  std::unique_ptr<DirectDeclaratorAST> _decl;  

  public:
  ParamDeclAST(std::unique_ptr<DeclSpecifierAST>& specs, std::unique_ptr<DirectDeclaratorAST>& decl) : _specs(std::move(specs)), _decl(std::move(decl)) {}   
};

class DeclarationAST : public NodeAST {

};

class FunctionDefinitionAST : public  NodeAST {
  
};

enum TypeQualifier {
  CONST,
  RESTRICT,
  VOLATILE,
  ATOMIC
};

class DeclSpecifierAST : public NodeAST {
  std::unique_ptr<TypeQualifier> _qualifier;
  std::unique_ptr<TypeSpecifier> _specifier;

  public:
  DeclSpecifierAST(std::unique_ptr<TypeQualifier>& qualifier, std::unique_ptr<TypeSpecifier>& specifier) : _qualifier(std::move(qualifier)), _specifier(std::move(specifier)) {}
};
