#include <string>
#include <vector>
#include <memory>
#include <iostream>

using std::cout;
using std::endl;

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
  std::vector<ExternalDecls*> _extern_units;    

  public:
  void insertExternalUnit(ExternalDecls* &unit) {
    _extern_units.push_back(unit);
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
  ExprAST *left, *right;

  public:
  BinaryExprAST(const std::string& op, ExprAST* LHS, ExprAST* RHS)
    : op(op), left(LHS), right(RHS) {}

  void print(int indent) { 
    left->print(indent+1);
    printIndent(indent); 
    printf("%s\n", op);
    right->print(indent+1);
  }
};

class ExprStmtAST : public StmtAST {
  ExprAST* _expression;

  public:
  ExprStmtAST(ExprAST*& expression) : _expression(expression) {  }
  ExprStmtAST() : _expression(nullptr) {  }

  void print(int indent) {
    if (_expression != nullptr) _expression->print(indent);
  }
};

class BlockItemListAST : public StmtAST {
  std::vector<StmtAST*> _statements;

  public:
  BlockItemListAST() {}

  void insertStatement(StmtAST*& stmt) {
    _statements.push_back(stmt);
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
  /* void updatePointer(PointerAst> &ptrast) { _pointer = ptrast; } */
};

class SpecifierAST : public NodeAST {
};

class DeclSpecifierAST : public NodeAST {
  SpecifierAST* _cur_specifier;
  DeclSpecifierAST* _next;

  public:
  DeclSpecifierAST(SpecifierAST* &cur_specifier, DeclSpecifierAST* &next) :
    _cur_specifier(cur_specifier), _next(next) {  }

  DeclSpecifierAST(SpecifierAST* &cur_specifier) : _cur_specifier ( cur_specifier)  {  }

  void print(int indent) {
    _cur_specifier->print(indent);
    if (_next != nullptr) _next->print(indent);
  }

};

class ParamDeclAST : public NodeAST {
  DeclSpecifierAST* _specs;
  DirectDeclaratorAST* _decl;  

  public:
  ParamDeclAST(DeclSpecifierAST*& specs, DirectDeclaratorAST*& decl) : _specs(specs), _decl(decl) {}   

  ParamDeclAST(DeclSpecifierAST*& specs) : _specs(specs) {}  

  void print(int indent) {
    printIndent(indent);
    printf("Parameter\n");
    if (_specs != nullptr) _specs->print(indent+1);
    if (_decl != nullptr) _decl->print(indent+1);
  }
};

class ParamListAST : public NodeAST {
  bool _isEllipsis;
  std::vector<ParamDeclAST*> _params;

  public:
  ParamListAST(bool isellipsis) : _isEllipsis(isellipsis) {  }

  void insertParam(ParamDeclAST*& param) {
    _params.push_back(param);
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
  DirectDeclaratorAST* _identifier;
  ParamListAST* _paramlist;

  public:
  FunctionDeclaratorAST(DirectDeclaratorAST* &identifier_decl, ParamListAST* &paramlist) :
    _identifier(identifier_decl), _paramlist(paramlist) {  }

  void print(int indent) {
    printIndent(indent);
    printf("FunctionDeclarator\n");
    if (_identifier != nullptr) _identifier->print(indent+1);
    if (_paramlist != nullptr) _paramlist->print(indent+1);
  }
};

class IdDeclaratorAST : public DirectDeclaratorAST {
  std::string _name;

  public:
  IdDeclaratorAST(const std::string& name) : _name(name) { cout << "name" << name << endl; }

  void print(int indent) {
    printIndent(indent);
    printf("ID:%s\n", _name);
  }
};



class DeclarationAST : public NodeAST {

};


class FunctionDefinitionAST : public ExternalDecls {
  DeclSpecifierAST* _decl_specs;
  DirectDeclaratorAST* _declarators;
  BlockItemListAST* _compound_stmts;

  public:
  FunctionDefinitionAST(DeclSpecifierAST* &declspecs, DirectDeclaratorAST* &decls, BlockItemListAST* &stmts) :
    _decl_specs(declspecs), _declarators(decls), _compound_stmts(stmts) {  }

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

