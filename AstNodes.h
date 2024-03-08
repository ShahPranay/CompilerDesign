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

class ExternalDecls : public NodeAST {
};

class RootAST : public NodeAST {
  std::vector<std::unique_ptr<ExternalDecls>> _extern_units;    

  public:
  void insertExternalUnit(std::unique_ptr<ExternalDecls> &unit) {
    _extern_units.push_back(std::move(unit));
  }

  void print(int indent) {
    printIndent(indent);
    printf("External units\n");
    for(int i=0 ;i<_extern_units.size(); i++) {
      _extern_units[i]->print(indent+1);
    }
  }
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
    if (_expression != nullptr) _expression->print(indent);
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
  /* PointerAst _pointer; // to write */

  public:
  /* void updatePointer(std::unique_ptr<PointerAst> &ptrast) { _pointer = ptrast; } */
};

class SpecifierAST : public NodeAST {
};

class DeclSpecifierAST : public NodeAST {
  std::unique_ptr<SpecifierAST> _cur_specifier;
  std::unique_ptr<DeclSpecifierAST> _next;

  public:
  DeclSpecifierAST(std::unique_ptr<SpecifierAST> &cur_specifier, std::unique_ptr<DeclSpecifierAST> &next) :
    _cur_specifier(std::move(cur_specifier)), _next(std::move(next)) {  }

  DeclSpecifierAST(std::unique_ptr<SpecifierAST> &cur_specifier) : _cur_specifier ( std::move(cur_specifier) ) {  }

  void print(int indent) {
    _cur_specifier->print(indent);
    if (_next != nullptr) _next->print(indent);
  }

};

class ParamDeclAST : public NodeAST {
  std::unique_ptr<DeclSpecifierAST> _specs;
  std::unique_ptr<DirectDeclaratorAST> _decl;  

  public:
  ParamDeclAST(std::unique_ptr<DeclSpecifierAST>& specs, std::unique_ptr<DirectDeclaratorAST>& decl) : _specs(std::move(specs)), _decl(std::move(decl)) {}   

  ParamDeclAST(std::unique_ptr<DeclSpecifierAST>& specs) : _specs(std::move(specs)) {}  

  void print(int indent) {
    printIndent(indent);
    printf("Parameter\n");
    if (_specs != nullptr) _specs->print(indent+1);
    if (_decl != nullptr) _decl->print(indent+1);
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
    if (_params.size()>0 || _isEllipsis == true) {
      printIndent(indent);
      printf("ParameterList\n");
    }
    for(int i=0;i<_params.size();i++) {
      _params[i]->print(indent+1);
    }
    if (_isEllipsis == true) {
      printIndent(indent+1);
      printf("...\n");
    }
  }
};

class FunctionDeclaratorAST : public DirectDeclaratorAST {
  std::unique_ptr<DirectDeclaratorAST> _identifier;
  std::unique_ptr<ParamListAST> _paramlist;

  public:
  FunctionDeclaratorAST(std::unique_ptr<DirectDeclaratorAST> &identifier_decl, std::unique_ptr<ParamListAST> &paramlist) :
    _identifier(std::move(identifier_decl)), _paramlist(std::move(paramlist)) {  }

  void print(int indent) {
    printIndent(indent);
    printf("FunctionDeclarator\n");
    if (_identifier != nullptr) _identifier->print(indent+1);
    if (_paramlist != nullptr) _paramlist->print(indent+1);
  }
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



class DeclarationAST : public NodeAST {

};


class FunctionDefinitionAST : public ExternalDecls {
  std::unique_ptr<DeclSpecifierAST> _decl_specs;
  std::unique_ptr<DirectDeclaratorAST> _declarators;
  std::unique_ptr<BlockItemListAST> _compound_stmts;

  public:
  FunctionDefinitionAST(std::unique_ptr<DeclSpecifierAST> &declspecs, std::unique_ptr<DirectDeclaratorAST> &decls, std::unique_ptr<BlockItemListAST> &stmts) :
    _decl_specs(std::move(declspecs)), _declarators(std::move(decls)), _compound_stmts(std::move(stmts)) {  }

  void print(int indent) {
    printIndent(indent);
    printf("Function Definition\n");
    if (_decl_specs != nullptr) _decl_specs->print(indent+1);
    if (_declarators != nullptr) _declarators->print(indent+1);
    if (_compound_stmts != nullptr) _compound_stmts->print(indent+1);
  }
};



class TypeQualifierAST : public SpecifierAST {
  std::string _qual_name;

  public:
  TypeQualifierAST(std::string name) : _qual_name(name) {  }

  void print(int indent) {
    printIndent(indent);
    printf("Type Qualifier : %s", _qual_name);
  }
};

class TypeSpecifierAST : public SpecifierAST {
};

class PrimitiveTypeSpecAST : public TypeSpecifierAST {
  std::string _type_name;
  
  public:
  PrimitiveTypeSpecAST(std::string type_name) : _type_name ( type_name ) {  }

  void print(int indent) {
    printIndent(indent);
    printf("Type Specifier : %s", _type_name);
  }
};

