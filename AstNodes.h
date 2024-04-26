#include "llvm/IR/Value.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Instructions.h"

#include <string>
#include <vector>
#include <memory>
#include <iostream>
#include <list>
//#include "llvm/IR/Value.h"
/* #include "ASTTypes.h" */

using std::cout;
using std::endl;

//Base class for all nodes
class NodeAST {
  public:
    virtual ~NodeAST() = default;
    virtual void print(int indent) { };

    void printIndent(int indent) {
      for(int i=0; i<indent ; i++) {
        cout << "|  ";
      }
      cout << "+-";
    }
};

class SpecifierAST : public NodeAST {
};

class TypeQualifierAST : public SpecifierAST {
  std::string _qual_name;

  public:
  TypeQualifierAST(std::string name) : _qual_name(name) {  }

  virtual void print(int indent) {
    printIndent(indent);
    cout << "Type Qualifier : " << _qual_name << endl;
  }
};

class TypeQualListAST : public NodeAST {
  std::vector<TypeQualifierAST*> _list;

  public:
  TypeQualListAST() {  }
  void insertQual(TypeQualifierAST* _qual) { _list.push_back(_qual); }

  virtual void print(int indent) {
    printIndent(indent);
    cout << "Type Qualifier List " << endl;
    indent++;
    for( auto ptr : _list )
    {
      ptr->print(indent);
    }
  }
};

class TypeSpecifierAST : public SpecifierAST {
};

class PrimitiveTypeSpecAST : public TypeSpecifierAST {
  std::string _type_name;

  public:
  PrimitiveTypeSpecAST(std::string type_name) : _type_name ( type_name ) { }

  virtual void print(int indent) {
    printIndent(indent);
    cout << "Type Specifier : " << _type_name << endl;
  }

  std::string getName() { return _type_name; }
};

class PointerAST : public NodeAST {
  std::list<TypeQualListAST*> _pointer_list;

  public:
  void pushFrontPointer(TypeQualListAST* curtql) { _pointer_list.push_front(curtql); }
  void pushFrontPointer() { _pointer_list.push_front(nullptr); }

  virtual void print(int indent) 
  {
    printIndent(indent);
    cout << "Pointer " << endl;
    indent++;
    for( auto ptr: _pointer_list )
    {
      printIndent(indent);
      cout << "* " << endl;
      if (ptr)
        ptr->print(indent);
    }
  }
};

class DeclSpecifiersAST : public NodeAST {
  std::list<SpecifierAST*> _decl_specs;

  public:
  DeclSpecifiersAST(SpecifierAST* cur_specifier) { _decl_specs.push_back(cur_specifier); }
  void pushFrontSpecifier(SpecifierAST* nxt_spec) { _decl_specs.push_front(nxt_spec); }

  virtual void print(int indent) {
    printIndent(indent);
    cout << "Declaration Specifiers " << endl;
    indent++;
    for(auto ptr: _decl_specs)
    {
      ptr->print(indent);
    }
  }

  llvm::Type* getLLVMType();

};

// make PointerAST and DeclSpecifiersAST lightweight (for easy copying). 
enum BaseType
{
  CONSTANT, VOID, CHAR, SHORT, INT, LONG, FLOAT, DOUBLE, SIGNED, UNSIGNED, BOOL,
};

enum Qualifier
{
  EMPTY, CONST,
};

class TypeInfo {
  BaseType _basetype;
  Qualifier _basequalifier;
  std::vector<Qualifier> _ptrinfo;

  public:
  TypeInfo(DeclSpecifiersAST *declspecs, PointerAST *ptr);
  TypeInfo(BaseType bt, Qualifier bql = Qualifier::EMPTY) : _basetype(bt), _basequalifier(bql) {  };
  TypeInfo() : _basetype(BaseType::CONSTANT), _basequalifier(Qualifier::EMPTY) {  }

  bool iscompatible(TypeInfo *other);
  llvm::Type *getLLVMType();
};

class FunctionTypeInfo {
  TypeInfo *_retType;
  std::vector<TypeInfo*> _paramTypes;

  public:
  FunctionTypeInfo(TypeInfo *rettype, std::vector<TypeInfo *> paramtypes) : _retType(rettype), _paramTypes(paramtypes) {  }
  TypeInfo *getReturnTypeInfo() { return _retType; };
  std::vector<TypeInfo*>& getParamTypeInfos();
  std::vector<llvm::Type*> getLLVMParamTypes();
};

class VarData
{
  public:
  TypeInfo *type;
  llvm::AllocaInst *allocainst;

  VarData() : type(nullptr), allocainst(nullptr) {  }
  VarData(TypeInfo *typepar, llvm::AllocaInst *allocainstpar) : type(type), allocainst(allocainstpar) {  }
};

class ExprRet {
  TypeInfo *_type;
  llvm::Value *_expr_value;

  public:
  ExprRet (TypeInfo *type, llvm::Value *val) : _type(type), _expr_value(val) {  }
  TypeInfo *getType() { return _type; }
  llvm::Value *getValue() { return _expr_value; } 
};

class ExternalDeclsAST : public NodeAST {
  public:
    virtual void codegen() = 0;
};

class RootAST : public NodeAST {
  std::vector<ExternalDeclsAST*> _extern_units;    

  public:
  void codegen();
  void insertExternalUnit(ExternalDeclsAST* unit) {
    _extern_units.push_back(unit);
  }

  virtual void print(int indent) {
    printIndent(indent);
    cout << "External Units" << endl;
    for(int i=0 ;i<_extern_units.size(); i++) {
      _extern_units[i]->print(indent+1);
    }
  }
};


//Base class for all exressions
class ExprAST : public NodeAST {
  public:
  virtual ExprRet* codegen() = 0;
};

class BlockItemAST : public NodeAST {

  public:
    virtual void codegen() = 0;
};

// Base class for all statements
class StmtAST : public BlockItemAST {

};

class IntegerExprAST : public ExprAST {
  int _val;

  public:
  IntegerExprAST(int Val) : _val(Val) {  }

  virtual ExprRet* codegen() override;

  virtual void print(int indent) { 
    printIndent(indent);
    cout << "Val : " << _val << endl; 
  }
};

class DoubleExprAST : public ExprAST {
  double _val;

  public:
  DoubleExprAST(double Val) : _val(Val) {}

  virtual ExprRet* codegen() override;

  virtual void print(int indent) { 
    printIndent(indent); 
    cout << "Val : " << _val << endl; 
  }
};

class StrLiteralAST : public ExprAST {
  std::string _str;

  public:
  StrLiteralAST(const std::string& str) : _str ( str ) {  }

  ExprRet* codegen() override;
  void print(int indent) {
    printIndent(indent);
    cout << "Str Literal: " << _str << endl;
  }
};

class IdentifierAST : public ExprAST {
  std::string _name;

  public:
  IdentifierAST(const std::string& name) : _name( name ) {  }

  ExprRet* codegen() override;
  VarData getVarData();
  virtual std::string getName() { return _name; }

  virtual void print(int indent) { 
    printIndent(indent); 
    cout << "ID : " << _name << endl; 
  }
};

// Binary expression AST node
class BinaryExprAST : public ExprAST {
  std::string op;
  ExprAST *left, *right;

  public:
  BinaryExprAST(const std::string& op, ExprAST* LHS, ExprAST* RHS)
    : op(op), left(LHS), right(RHS) {}

  virtual ExprRet* codegen() override;

  virtual void print(int indent) { 
    left->print(indent+1);

    printIndent(indent); 
    cout << "Op : " << op << endl;

    right->print(indent+1);
  }
};

class ExprStmtAST : public StmtAST {
  ExprAST* _expression;

  public:
  ExprStmtAST(ExprAST* expression) : _expression(expression) {  }
  ExprStmtAST() : _expression(nullptr) {  }

  virtual void print(int indent) {
    if (_expression != nullptr) _expression->print(indent);
  }

  void codegen() override;
};

class IfElseStmtAST : public StmtAST {
  ExprAST* _expression;
  StmtAST* _then_statement;
  StmtAST* _else_statement;

  public:
  IfElseStmtAST(ExprAST* expression, StmtAST* then_statement) : _expression(expression), _then_statement(then_statement), _else_statement(nullptr) {  }
  IfElseStmtAST(ExprAST* expression, StmtAST* then_statement, StmtAST* else_statement) : _expression(expression), _then_statement(then_statement), _else_statement(else_statement) { }

  virtual void print(int indent) {
    printIndent(indent);
    cout << "If Condition\n";
    _expression->print(indent+1);
    printIndent(indent);
    cout << "Then Block\n";
    _then_statement->print(indent+1);
    if (_else_statement != nullptr) {
      printIndent(indent);
      cout << "Else Block\n";
      _else_statement->print(indent+1);
    }
  }

  void codegen() override;
};

class WhileStmtAST : public StmtAST {
  ExprAST* _expression;
  StmtAST* _statement;

  public:
  WhileStmtAST(ExprAST* expression, StmtAST* statement) : _expression(expression), _statement(statement) { }

  virtual void print(int indent) {
    printIndent(indent);
    cout << "While Condition\n";
    _expression->print(indent+1);
    _statement->print(indent);
  } 

  void codegen() override;
};

class GotoStmtAST : public StmtAST {
  IdentifierAST* _identifier;

  public:
  GotoStmtAST(const std::string& identifier) : _identifier(new IdentifierAST(identifier)) { }

  virtual void print(int indent) {
    printIndent(indent);
    cout << "GOTO\n";
    _identifier->print(indent+1);
  }

  void codegen() override;
};

class ReturnStmtAST : public StmtAST {
  ExprAST* _expr; 

  public:
  ReturnStmtAST() : _expr(nullptr) {  }
  ReturnStmtAST(ExprAST* expr) : _expr(expr) {  }

  virtual void print(int indent) {
    printIndent(indent);
    cout << "Return\n";
    if (_expr)
      _expr->print(indent);
  }

  void codegen() override;
};

class ArgListAST : public NodeAST {
  std::vector<ExprAST*> _arg_list;

  public:
  ArgListAST() { }
  void insertArg(ExprAST* arg) { _arg_list.push_back(arg); }

  virtual void print(int indent) {
    for(int i=0; i<_arg_list.size() ; i++) {
      printIndent(indent);
      cout << "Arg " << i << endl;
      _arg_list[i]->print(indent+1);
    }
  }

  std::vector<ExprAST*> getArgs() { return _arg_list; }
  int getSize() { return _arg_list.size(); }
}; 

class FunctionCallAST : public ExprAST {
  ExprAST* _name;
  ArgListAST* _argument_list;

  public:
  FunctionCallAST(ExprAST* name, ArgListAST* argument_list) : _name(name), _argument_list(argument_list) { }
  FunctionCallAST(ExprAST* name) : _name(name), _argument_list(nullptr) { }

  virtual void print(int indent) {
    printIndent(indent);
    cout << "Function Call " << endl;
    _name->print(indent+1);
    if (_argument_list != nullptr) _argument_list->print(indent+1);
  }

  virtual ExprRet* codegen() override;
  int getSize() { return _argument_list->getSize(); }
  std::vector<ExprAST*> getArgs() { return _argument_list->getArgs(); }
};

class BlockItemListAST : public StmtAST {
  std::vector<BlockItemAST*> _items;

  public:
  BlockItemListAST() {}

  void insertBlockItem(BlockItemAST *item) {
    _items.push_back(item);
  }

  virtual void print(int indent) {
    for(int i = 0; i<_items.size();i++) {
      printIndent(indent);
      cout << "Statement " << i+1 << endl;
      _items[i]->print(indent+1);
    }
  }

  bool isReturnPresent() { 
    bool present = false;
    for(auto item: _items) {
      if (dynamic_cast<ReturnStmtAST*>(item)) {
        present = true;
      }
    }
    return present;
  }

  void codegen();
};



class DirectDeclaratorAST : public NodeAST {
  protected:
    PointerAST *_pointer;

  public:
    DirectDeclaratorAST() : _pointer(nullptr) {  }
    void updatePointer(PointerAST* ptr) { _pointer = ptr; }
    virtual void codegen(DeclSpecifiersAST *specs ) = 0;
    virtual std::string getName() = 0;
};

class ParamDeclAST : public NodeAST {
  DeclSpecifiersAST* _specs;
  DirectDeclaratorAST* _decl;  

  public:
  ParamDeclAST(DeclSpecifiersAST* specs, DirectDeclaratorAST* decl) : 
    _specs(specs), _decl(decl) {}   

  ParamDeclAST(DeclSpecifiersAST* specs) : _specs(specs) {}  

  virtual void print(int indent) {
    printIndent(indent);
    cout << "Parameter" << endl;
    indent++;

    if (_specs != nullptr) _specs->print(indent);
    if (_decl != nullptr) _decl->print(indent);
  }

  llvm::Type* getLLVMType() { return _specs->getLLVMType(); }
  std::string getName() { return _decl->getName(); }
};

class ParamListAST : public NodeAST {
  bool _isEllipsis;
  std::vector<ParamDeclAST*> _params;

  public:
  ParamListAST(bool isellipsis) : _isEllipsis(isellipsis) {  }

  void insertParam(ParamDeclAST* param) {
    _params.push_back(param);
  }

  void updateEllipsis(bool isellipsis) {
    _isEllipsis = isellipsis;
  }

  virtual void print(int indent) {
    if (_params.size()>0 || _isEllipsis == true) {
      printIndent(indent);
      cout << "ParameterList" << endl;
    }

    for(int i=0;i<_params.size();i++) {
      _params[i]->print(indent+1);
    }

    if (_isEllipsis == true) {
      printIndent(indent+1);
      cout << "Ellipsis" << endl;
    }
  }

  std::vector<TypeInfo *> getParamTypeInfos();
  std::vector<llvm::Type*> getParamTypes();
  std::vector<std::string> getParamNames();
};

class FunctionDeclaratorAST : public DirectDeclaratorAST {
  DirectDeclaratorAST* _identifier;
  ParamListAST* _paramlist;

  public:
  FunctionDeclaratorAST(DirectDeclaratorAST* identifier_decl, ParamListAST* paramlist) :
    _identifier(identifier_decl), _paramlist(paramlist) {  }

  virtual void print(int indent) {
    printIndent(indent);
    cout << "Function Declarator" << endl;
    indent++;

    if (_pointer) _pointer->print(indent);
    if (_identifier != nullptr) _identifier->print(indent);
    if (_paramlist != nullptr) _paramlist->print(indent);
  }

  virtual std::string getName() override { return _identifier->getName(); }

  void codegen(DeclSpecifiersAST* specs) override;
};

class IdDeclaratorAST : public DirectDeclaratorAST {
  std::string _name;

  public:
  IdDeclaratorAST(const std::string& name) : _name(name) { }

  virtual void print(int indent) {
    printIndent(indent);
    cout << "ID Declarator " << endl;
    indent++;

    if (_pointer)
      _pointer->print(indent);
    printIndent(indent);
    cout << _name << endl;
  }

  virtual std::string getName() override { return _name; }
  llvm::AllocaInst* getAlloca();
  virtual void codegen(DeclSpecifiersAST *specs) override;
};

class FunctionDefinitionAST : public ExternalDeclsAST {
  DeclSpecifiersAST* _decl_specs;
  DirectDeclaratorAST* _func_declarator;
  BlockItemListAST* _compound_stmts;

  public:
  FunctionDefinitionAST(DeclSpecifiersAST* declspecs, DirectDeclaratorAST* decls, BlockItemListAST* stmts) :
    _decl_specs(declspecs), _func_declarator(decls), _compound_stmts(stmts) {  }

  virtual void print(int indent) {
    printIndent(indent);
    cout << "Function Definition" << endl;

    if (_decl_specs != nullptr) _decl_specs->print(indent+1);
    if (_func_declarator != nullptr) _func_declarator->print(indent+1);
    if (_compound_stmts != nullptr) _compound_stmts->print(indent+1);
  }

  bool isReturnPresent() { return _compound_stmts->isReturnPresent(); }

  void codegen() override;

};

class DesignationAST : public NodeAST {
};

class DesignatorAST : public NodeAST {

};

class ArrayDesignatorAST : public DesignatorAST {
  ExprAST* _expr;

  public:
  ArrayDesignatorAST(ExprAST* expr) : _expr(expr) { }

  virtual void print(int indent) {
    _expr->print(indent);
  }
};

class IdDesignatorAST : public DesignatorAST {
  std::string _id; 

  public:
    IdDesignatorAST(const std::string& id) : _id (id) {  }

    void print(int indent)
    {
      printIndent(indent);
      cout << _id << endl;
    }
};

class DesignatorListAST : public DesignationAST {
  std::vector<DesignatorAST*> _designator_list;

  public:
  DesignatorListAST() { }
  void insertDesignator(DesignatorAST* designator) { _designator_list.push_back(designator); }

  virtual void print(int indent) {
    for(int i=0; i<_designator_list.size(); i++) {
      printIndent(indent);
      cout << "Designator " << i+1 << endl;
      _designator_list[i]->print(indent+1); 
    }
  }

};


class InitializerAST : public NodeAST {
  DesignationAST *_designation;
  ExprAST* _assignment_expression;

  public:
  InitializerAST() : _designation(nullptr), _assignment_expression(nullptr) {  }
  InitializerAST(ExprAST* ass_expr) : _designation(nullptr), _assignment_expression(ass_expr) { }  

  void setDesignation(DesignationAST *des) { _designation = des; }

  virtual void print(int indent) {
    if (_designation) _designation->print(indent);
    _assignment_expression->print(indent);
  }

  virtual ExprRet* codegen();
};

class InitializerListAST : public InitializerAST {
  std::vector<InitializerAST *> _initializer_list;

  public:
  InitializerListAST() { }
  void insertElem(InitializerAST *item) { _initializer_list.push_back(item); }

  virtual void print(int indent) {
    /* for (int i = 0; i < _initializer_list.size(); i++) { */
    /*   printIndent(indent); */
    /*   cout << "Initializer List Element " << i+1 << endl; */
    /*   _initializer_list[i]->print(indent+1); */
    /* } */
  }

  ExprRet* codegen() override;
};

class InitDeclaratorAST : public NodeAST {
  DirectDeclaratorAST* _direct_decl;
  InitializerAST* _initializer;

  public:
  InitDeclaratorAST(DirectDeclaratorAST* ddecl) : _direct_decl(ddecl), _initializer(nullptr) {  }
  InitDeclaratorAST(DirectDeclaratorAST* ddecl, InitializerAST* initdecl) : _direct_decl(ddecl), _initializer(initdecl) {  }

  virtual void print(int indent) {
    printIndent(indent);
    cout << "Init Declarator" << endl;
    indent++;
    _direct_decl->print(indent);

    if (_initializer) {
      printIndent(indent);
      cout << "Op : =" << endl;
      _initializer->print(indent+1);
    } 
  }

  void codegen(llvm::Type* codegen);
};

class InitDeclaratorListAST : public NodeAST {
  std::vector<InitDeclaratorAST*> _init_declarators;

  public:
  InitDeclaratorListAST() {  }
  void insertInitDeclarator(InitDeclaratorAST* cur_init_decl) { _init_declarators.push_back(cur_init_decl); }

  virtual void print(int indent) {
    printIndent(indent);
    cout << "Init Declarators" << endl;
    indent++;
    for(int i=0; i<_init_declarators.size(); i++) {
      _init_declarators[i]->print(indent);
    }
  }

  void codegen(llvm::Type* codegen);

};

class DeclarationAST : public ExternalDeclsAST {
  public:
    virtual void codegen() = 0;
};

class NormalDeclAST : public DeclarationAST {
  DeclSpecifiersAST* _specs;
  InitDeclaratorListAST* _init_decl_list;

  public:
  NormalDeclAST(DeclSpecifiersAST* declspecs) : _specs(declspecs), _init_decl_list(nullptr) {  }
  NormalDeclAST(DeclSpecifiersAST* declspecs, InitDeclaratorListAST* init_list) : _specs(declspecs), _init_decl_list(init_list) {  }
  // DeclSpecifierAST
  // list of init_declarator
  virtual void print(int indent) {
    printIndent(indent);
    cout << "Declaration " << endl;
    indent++;
    _specs->print(indent);
    if (_init_decl_list != nullptr) _init_decl_list->print(indent);
  }
  virtual void codegen() override;
};

class StaticAssertDeclAST : public DeclarationAST {

};

