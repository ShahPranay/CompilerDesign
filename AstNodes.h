#include <string>
#include <vector>
#include <memory>

//Base class for all nodes
class NodeAST {
  public:
    virtual ~NodeAST() = default;
    virtual void print(int indent) { };
    void printIndent(int indent) {
      for(int i=0; i<indent ; i++) {
        printf("|  ");
    }
    printf("+-");
  }
};

class RootAST {
   
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

  void print(int indent) { printIndent(indent); printf("%d\n", val); }
};

class DoubleExprAST : public ExprAST {
  double val;

  public:
  DoubleExprAST(double Val) : val(Val) {}

  void print(int indent) { printIndent(indent); printf("%lf\n", val); }
};

class IdentifierAST : public ExprAST {
  std::string _name;

  public:
  IdentifierAST(const std::string& name) : _name( name ) {  }

  void print(int indent) { printIndent(indent); printf("%s\n", _name); }
};

// Binary expression AST node
class BinaryExprAST : public ExprAST {
  std::string op;
  std::unique_ptr<ExprAST> left, right;

  public:
  BinaryExprAST(const std::string& op, std::unique_ptr<ExprAST> LHS, std::unique_ptr<ExprAST> RHS)
    : op(op), left(std::move(LHS)), right(std::move(RHS)) {}

  void print(int indent) { 
    left->print(indent+1);
    printIndent(indent); 
    printf("%s\n", op);
    right->print(indent+1);
  }
};

class ExprStmtAST : public StmtAST {
  std::unique_ptr<ExprAST> _expression;

  public:
  ExprStmtAST(std::unique_ptr<ExprAST>& expression) : _expression(std::move(expression)) {  }
  ExprStmtAST() : _expression(std::make_unique<ExprAST>(nullptr)) {  }

  void print(int indent) {
    _expression->print(indent);
  }
};

class BlockItemListAST : public StmtAST {
  std::vector<std::unique_ptr<StmtAST>> _statements;

  public:
  BlockItemListAST() {}
  void insertStatement(std::unique_ptr<StmtAST>& stmt) {
    _statements.push_back(std::move(stmt));
  }

  void print(int indent) {
    for(int i = 0; i<_statements.size();i++) {
      _statements[i]->print(indent);
    }
  }
};

class DirectDeclaratorAST : public NodeAST { 
  PointerAst _pointer; // to write
};

class FunctionDeclaratorAST : public DirectDeclaratorAST {
  std::unique_ptr<DirectDeclaratorAST> _identifier;
  std::unique_ptr<ParamListAST> _paramlist;

  public:
  FunctionDeclaratorAST(std::unique_ptr<DirectDeclaratorAST> &identifier_decl, std::unique_ptr<ParamListAST> &paramlist) :
    _identifier(identifier_decl), _paramlist(paramlist) {  }
};

class IdDeclaratorAST : public NodeAST {
  std::string _name;

  public:
  IdDeclaratorAST(const std::string& name) : _name(name) {  }

  void print(int indent) {
    printIndent(indent);
    printf("ID:%s\n", _name);
  }
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

  void print(int indent) {
    printIndent(indent);
    printf("ParameterList\n");
    for(int i=0;i<_params.size();i++) {
      _params[i]->print(indent+1);
    }
    if (_isEllipsis == true) {
      printIndent(indent+1);
      printf("...\n");
    }
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


class SpecifierAST : public NodeAST {
};

class TypeQualifierAST : public SpecifierAST {
  std::string _qual_name;

  public:
  TypeQualifierAST(std::string name) : _qual_name(name) {  }
};

class TypeSpecifierAST : public SpecifierAST {
};

class PrimitiveTypeSpecAST : public TypeSpecifierAST {
  std::string _type_name;
  
  public:
  PrimitiveTypeSpecAST(std::string type_name) : _type_name ( type_name ) {  }
};

class DeclSpecifierAST : public NodeAST {
  std::unique_ptr<SpecifierAST> _cur_specifier;
  std::unique_ptr<DeclSpecifierAST> _next;

  public:
  DeclSpecifierAST(std::unique_ptr<SpecifierAST> &cur_specifier, std::unique_ptr<DeclSpecifierAST> &next) :
    _cur_specifier(cur_specifier), _next(next) {  }
};
