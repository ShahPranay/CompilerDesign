%{
#include <cstdio>
#include <iostream>
#include "AstNodes.h"
using namespace std;

// stuff from flex that bison needs to know about:
extern "C" int yylex();
int yyparse();
extern "C" FILE *yyin;

void yyerror(const char *s);

RootAST* AST_root;
%}

%union{
  NodeAST* node;
  StmtAST* statement;
  ExprAST* expression;
  IdentifierAST* identifier;
  ArgListAST* arg_list;
  FunctionCallAST* function_call;
  BlockItemListAST* block_list;
  ParamListAST* param_list;
  ParamDeclAST* param_decl;
  DirectDeclaratorAST* direct_decl;
  SpecifierAST* specifier;
  DeclSpecifiersAST* decl_specs;
  ExternalDeclsAST* external_decls;
  DeclarationAST* declaration_ast;
  InitDeclaratorListAST* init_decl_list;
  InitDeclaratorAST* init_decl;
  InitializerAST* initializer_ast;
  InitializerListAST* initializer_list_ast;
  DesignationAST* designation_ast;
  DesignatorAST* designator_ast;
  DesignatorListAST* designator_list_ast;

  TypeQualListAST* type_qual_list;
  TypeQualifierAST* type_qual;

  PointerAST* ptr_ast;

  int int_token;
  double double_token;
  std::string* str_token;
}

%token	<str_token> IDENTIFIER I_CONSTANT F_CONSTANT STRING_LITERAL FUNC_NAME SIZEOF
%token	PTR_OP INC_OP DEC_OP LEFT_OP RIGHT_OP LE_OP GE_OP EQ_OP NE_OP
%token	AND_OP OR_OP MUL_ASSIGN DIV_ASSIGN MOD_ASSIGN ADD_ASSIGN
%token	SUB_ASSIGN LEFT_ASSIGN RIGHT_ASSIGN AND_ASSIGN
%token	XOR_ASSIGN OR_ASSIGN
%token	TYPEDEF_NAME ENUMERATION_CONSTANT

%token	TYPEDEF EXTERN STATIC AUTO REGISTER INLINE
%token	CONST RESTRICT VOLATILE
%token	BOOL CHAR SHORT INT LONG SIGNED UNSIGNED FLOAT DOUBLE VOID
%token	COMPLEX IMAGINARY 
%token	STRUCT UNION ENUM ELLIPSIS

%token	CASE DEFAULT IF ELSE SWITCH WHILE DO FOR GOTO CONTINUE BREAK RETURN

%token	ALIGNAS ALIGNOF ATOMIC GENERIC NORETURN STATIC_ASSERT THREAD_LOCAL


%type <statement> statement expression_statement jump_statement block_item selection_statement iteration_statement
%type <block_list> block_item_list compound_statement

%type <expression> cast_expression unary_expression postfix_expression primary_expression
%type <expression> multiplicative_expression additive_expression shift_expression relational_expression 
%type <expression> equality_expression and_expression exclusive_or_expression inclusive_or_expression constant_expression
%type <expression> logical_and_expression logical_or_expression conditional_expression assignment_expression expression 
%type <expression> constant string

%type <arg_list> argument_expression_list

%type <param_list> parameter_type_list parameter_list
%type <param_decl> parameter_declaration
%type <specifier> type_specifier storage_class_specifier function_specifier alignment_specifier
%type <type_qual> type_qualifier
%type <type_qual_list> type_qualifier_list
%type <direct_decl> direct_declarator declarator
%type <decl_specs> declaration_specifiers;
%type <external_decls> function_definition external_declaration
%type <declaration_ast> declaration
%type <init_decl_list> init_declarator_list
%type <init_decl> init_declarator
%type <initializer_ast> initializer 
%type <initializer_list_ast> initializer_list
%type <ptr_ast> pointer
%type <designation_ast> designation
%type <designator_ast> designator
%type <designator_list_ast> designator_list

%start translation_unit

%%


primary_expression
  : IDENTIFIER { $$ = new IdentifierAST(*$1);}
  | constant
  | string
  | '(' expression ')' { $$ = $2; }
  | generic_selection
  ;

constant
  : I_CONSTANT	{ $$ = new IntegerExprAST(atoi($1->c_str())); }	/* includes character_constant */
  | F_CONSTANT  { $$ = new DoubleExprAST(atof($1->c_str())); }
  | ENUMERATION_CONSTANT	/* after it has been defined as such */
  ;

enumeration_constant		/* before it has been defined as such */
  : IDENTIFIER
  ;

string
  : STRING_LITERAL
  | FUNC_NAME
  ;

generic_selection
  : GENERIC '(' assignment_expression ',' generic_assoc_list ')'
  ;

generic_assoc_list
  : generic_association
  | generic_assoc_list ',' generic_association
  ;

generic_association
  : type_name ':' assignment_expression
  | DEFAULT ':' assignment_expression
  ;

postfix_expression
  : primary_expression
  | postfix_expression '[' expression ']'
  | postfix_expression '(' ')' { $$ = new FunctionCallAST($1); }
  | postfix_expression '(' argument_expression_list ')' { $$ = new FunctionCallAST($1, $3); }
  | postfix_expression '.' IDENTIFIER
  | postfix_expression PTR_OP IDENTIFIER
  | postfix_expression INC_OP
  | postfix_expression DEC_OP
  | '(' type_name ')' '{' initializer_list '}'
  | '(' type_name ')' '{' initializer_list ',' '}'
  ;

argument_expression_list
  : assignment_expression { $$ = new ArgListAST(); $$->insertArg($1); }
  | argument_expression_list ',' assignment_expression { $$ = $1; $$->insertArg($3); }
  ;

unary_expression
  : postfix_expression
  | INC_OP unary_expression
  | DEC_OP unary_expression
  | unary_operator cast_expression
  | SIZEOF unary_expression
  | SIZEOF '(' type_name ')'
  | ALIGNOF '(' type_name ')'
  ;

unary_operator
  : '&'
  | '*'
  | '+'
  | '-'
  | '~'
  | '!'
  ;

cast_expression
  : unary_expression
  | '(' type_name ')' cast_expression
  ;

multiplicative_expression
  : cast_expression
  | multiplicative_expression '*' cast_expression { $$ = new BinaryExprAST("*", $1, $3); }
  | multiplicative_expression '/' cast_expression { $$ = new BinaryExprAST("/", $1, $3); }
  | multiplicative_expression '%' cast_expression { $$ = new BinaryExprAST("%", $1, $3); }
  ;

additive_expression
  : multiplicative_expression
  | additive_expression '+' multiplicative_expression { $$ = new BinaryExprAST("+", $1, $3); }
  | additive_expression '-' multiplicative_expression { $$ = new BinaryExprAST("-", $1, $3); }
  ;

shift_expression
  : additive_expression
  | shift_expression LEFT_OP additive_expression { $$ = new BinaryExprAST("<<", $1, $3); }
  | shift_expression RIGHT_OP additive_expression { $$ = new BinaryExprAST(">>", $1, $3); }
  ;

relational_expression
  : shift_expression
  | relational_expression '<' shift_expression { $$ = new BinaryExprAST("<", $1, $3); }
  | relational_expression '>' shift_expression { $$ = new BinaryExprAST(">", $1, $3); }
  | relational_expression LE_OP shift_expression { $$ = new BinaryExprAST("<=", $1, $3); }
  | relational_expression GE_OP shift_expression { $$ = new BinaryExprAST(">=", $1, $3); }
  ;

equality_expression
  : relational_expression
  | equality_expression EQ_OP relational_expression { $$ = new BinaryExprAST("==", $1, $3); }
  | equality_expression NE_OP relational_expression { $$ = new BinaryExprAST("!=", $1, $3); }
  ;

and_expression
  : equality_expression
  | and_expression '&' equality_expression { $$ = new BinaryExprAST("&", $1, $3); }
  ;

exclusive_or_expression
  : and_expression
  | exclusive_or_expression '^' and_expression { $$ = new BinaryExprAST("^", $1, $3); }
  ;

inclusive_or_expression
  : exclusive_or_expression
  | inclusive_or_expression '|' exclusive_or_expression { $$ = new BinaryExprAST("|", $1, $3); }
  ;

logical_and_expression
  : inclusive_or_expression
  | logical_and_expression AND_OP inclusive_or_expression { $$ = new BinaryExprAST("&&", $1, $3); }
  ;

logical_or_expression
  : logical_and_expression
  | logical_or_expression OR_OP logical_and_expression { $$ = new BinaryExprAST("||", $1, $3); }
  ;

conditional_expression
  : logical_or_expression
  | logical_or_expression '?' expression ':' conditional_expression
  ;

assignment_expression
  : conditional_expression
  | unary_expression assignment_operator assignment_expression { $$ = new BinaryExprAST("=", $1, $3); /* TODO: enum class for assignment_operator */}
  ;

assignment_operator
  : '='
  | MUL_ASSIGN
  | DIV_ASSIGN
  | MOD_ASSIGN
  | ADD_ASSIGN
  | SUB_ASSIGN
  | LEFT_ASSIGN
  | RIGHT_ASSIGN
  | AND_ASSIGN
  | XOR_ASSIGN
  | OR_ASSIGN
  ;

expression
  : assignment_expression
  | expression ',' assignment_expression ;

constant_expression
  : conditional_expression	/* with constraints */
  ;

declaration
  : declaration_specifiers ';' { $$ = new NormalDeclAST($1); }
  | declaration_specifiers init_declarator_list ';' { $$ = new NormalDeclAST($1, $2); }
  | static_assert_declaration
  ;

declaration_specifiers
  : storage_class_specifier declaration_specifiers { $$ = $2; $$->pushFrontSpecifier($1); }
  | storage_class_specifier { $$ = new DeclSpecifiersAST($1); } 
  | type_specifier declaration_specifiers { $$ = $2; $$->pushFrontSpecifier($1); }
  | type_specifier { $$ = new DeclSpecifiersAST($1); }
  | type_qualifier declaration_specifiers { $$ = $2; $$->pushFrontSpecifier($1); }
  | type_qualifier { $$ = new DeclSpecifiersAST($1); }
  | function_specifier declaration_specifiers { $$ = $2; $$->pushFrontSpecifier($1); }
  | function_specifier { $$ = new DeclSpecifiersAST($1); }
  | alignment_specifier declaration_specifiers { $$ = $2; $$->pushFrontSpecifier($1); }
  | alignment_specifier { $$ = new DeclSpecifiersAST($1); }
  ;

init_declarator_list
  : init_declarator { $$ = new InitDeclaratorListAST(); $$->insertInitDeclarator($1);}
  | init_declarator_list ',' init_declarator { $$ = $1; $$->insertInitDeclarator($3); }
  ;

init_declarator
  : declarator '=' initializer { $$ = new InitDeclaratorAST($1, $3); }
  | declarator { $$ = new InitDeclaratorAST($1); }
  ;

storage_class_specifier
  : TYPEDEF	/* identifiers must be flagged as TYPEDEF_NAME */
  | EXTERN
  | STATIC
  | THREAD_LOCAL
  | AUTO
  | REGISTER
  ;

type_specifier
  : VOID { $$ = new PrimitiveTypeSpecAST("void"); }
  | CHAR { $$ = new PrimitiveTypeSpecAST("char"); }
  | SHORT { $$ = new PrimitiveTypeSpecAST("short"); }
  | INT { $$ = new PrimitiveTypeSpecAST("int"); }
  | LONG { $$ = new PrimitiveTypeSpecAST("long"); }
  | FLOAT { $$ = new PrimitiveTypeSpecAST("float"); }
  | DOUBLE { $$ = new PrimitiveTypeSpecAST("double"); }
  | SIGNED { $$ = new PrimitiveTypeSpecAST("signed"); }
  | UNSIGNED { $$ = new PrimitiveTypeSpecAST("unsigned"); }
  | BOOL { $$ = new PrimitiveTypeSpecAST("bool"); }
  | COMPLEX { $$ = new PrimitiveTypeSpecAST("complex"); }
  | IMAGINARY	  	/* non-mandated extension */
  | atomic_type_specifier
  | struct_or_union_specifier
  | enum_specifier
  | TYPEDEF_NAME		/* after it has been defined as such */
  ;

struct_or_union_specifier
  : struct_or_union '{' struct_declaration_list '}'
  | struct_or_union IDENTIFIER '{' struct_declaration_list '}'
  | struct_or_union IDENTIFIER
  ;

struct_or_union
  : STRUCT
  | UNION
  ;

struct_declaration_list
  : struct_declaration
  | struct_declaration_list struct_declaration
  ;

struct_declaration
  : specifier_qualifier_list ';'	/* for anonymous struct/union */
  | specifier_qualifier_list struct_declarator_list ';'
  | static_assert_declaration
  ;

specifier_qualifier_list
  : type_specifier specifier_qualifier_list
  | type_specifier
  | type_qualifier specifier_qualifier_list
  | type_qualifier
  ;

struct_declarator_list
  : struct_declarator
  | struct_declarator_list ',' struct_declarator
  ;

struct_declarator
  : ':' constant_expression
  | declarator ':' constant_expression
  | declarator
  ;

enum_specifier
  : ENUM '{' enumerator_list '}'
  | ENUM '{' enumerator_list ',' '}'
  | ENUM IDENTIFIER '{' enumerator_list '}'
  | ENUM IDENTIFIER '{' enumerator_list ',' '}'
  | ENUM IDENTIFIER
  ;

enumerator_list
  : enumerator
  | enumerator_list ',' enumerator
  ;

enumerator	/* identifiers must be flagged as ENUMERATION_CONSTANT */
  : enumeration_constant '=' constant_expression
  | enumeration_constant
  ;

atomic_type_specifier
  : ATOMIC '(' type_name ')'
  ;

type_qualifier
  : CONST { $$ = new TypeQualifierAST("const"); }
  | RESTRICT { $$ = new TypeQualifierAST("restrict"); }
  | VOLATILE { $$ = new TypeQualifierAST("volatile"); }
  | ATOMIC { $$ = new TypeQualifierAST("atomic"); }
  ;

function_specifier
  : INLINE
  | NORETURN
  ;

alignment_specifier
  : ALIGNAS '(' type_name ')'
  | ALIGNAS '(' constant_expression ')'
  ;

declarator
  : pointer direct_declarator { $$ = $2; $$->updatePointer($1); }
  | direct_declarator
  ;

direct_declarator
  : IDENTIFIER { $$ = new IdDeclaratorAST(*$1); }
  | '(' declarator ')'
  | direct_declarator '[' ']'
  | direct_declarator '[' '*' ']'
  | direct_declarator '[' STATIC type_qualifier_list assignment_expression ']'
  | direct_declarator '[' STATIC assignment_expression ']'
  | direct_declarator '[' type_qualifier_list '*' ']'
  | direct_declarator '[' type_qualifier_list STATIC assignment_expression ']'
  | direct_declarator '[' type_qualifier_list assignment_expression ']'
  | direct_declarator '[' type_qualifier_list ']'
  | direct_declarator '[' assignment_expression ']'
  | direct_declarator '(' parameter_type_list ')' { $$ = new FunctionDeclaratorAST($1, $3); }
  | direct_declarator '(' ')' { $$ = new FunctionDeclaratorAST($1, new ParamListAST(false)); }
  | direct_declarator '(' identifier_list ')'
  ;

pointer
  : '*' type_qualifier_list pointer { $$ = $3; $$->pushFrontPointer($2); }
  | '*' type_qualifier_list { $$ = new PointerAST(); $$->pushFrontPointer($2); }
  | '*' pointer { $$ = $2; $$->pushFrontPointer(); }
  | '*' { $$ = new PointerAST(); $$->pushFrontPointer(); }
  ;

type_qualifier_list
  : type_qualifier { $$ = new TypeQualListAST(); $$->insertQual($1); }
  | type_qualifier_list type_qualifier { $$ = $1; $$->insertQual($2); }
  ;

parameter_type_list
  : parameter_list ',' ELLIPSIS { $1->updateEllipsis(true); $$ = $1; }
  | parameter_list 
  ;

parameter_list
  : parameter_declaration { $$ = new ParamListAST(false); $$->insertParam($1); }
  | parameter_list ',' parameter_declaration { $1->insertParam($3); $$ = $1; }
  ;

parameter_declaration
  : declaration_specifiers declarator { $$ = new ParamDeclAST($1, $2); }
  | declaration_specifiers abstract_declarator
  | declaration_specifiers { $$ = new ParamDeclAST($1); }
  ;

identifier_list
  : IDENTIFIER
  | identifier_list ',' IDENTIFIER
  ;

type_name
  : specifier_qualifier_list abstract_declarator
  | specifier_qualifier_list
  ;

abstract_declarator
  : pointer direct_abstract_declarator
  | pointer
  | direct_abstract_declarator
  ;

direct_abstract_declarator
  : '(' abstract_declarator ')'
  | '[' ']'
  | '[' '*' ']'
  | '[' STATIC type_qualifier_list assignment_expression ']'
  | '[' STATIC assignment_expression ']'
  | '[' type_qualifier_list STATIC assignment_expression ']'
  | '[' type_qualifier_list assignment_expression ']'
  | '[' type_qualifier_list ']'
  | '[' assignment_expression ']'
  | direct_abstract_declarator '[' ']'
  | direct_abstract_declarator '[' '*' ']'
  | direct_abstract_declarator '[' STATIC type_qualifier_list assignment_expression ']'
  | direct_abstract_declarator '[' STATIC assignment_expression ']'
  | direct_abstract_declarator '[' type_qualifier_list assignment_expression ']'
  | direct_abstract_declarator '[' type_qualifier_list STATIC assignment_expression ']'
  | direct_abstract_declarator '[' type_qualifier_list ']'
  | direct_abstract_declarator '[' assignment_expression ']'
  | '(' ')'
  | '(' parameter_type_list ')'
  | direct_abstract_declarator '(' ')'
  | direct_abstract_declarator '(' parameter_type_list ')'
  ;

initializer
  : '{' initializer_list '}' { $$ = $2; }
  | '{' initializer_list ',' '}' { $$ = $2; }
  | assignment_expression { $$ = new InitializerAST($1); }
  ;

initializer_list
  : designation initializer { $$ = new InitializerListAST(); $2->setDesignation($1); $$->insertElem($2);}
  | initializer { $$ = new InitializerListAST(); $$->insertElem($1); }
  | initializer_list ',' designation initializer { $$ = $1; $4->setDesignation($3); $$->insertElem($4); }
  | initializer_list ',' initializer { $$ = $1; $$->insertElem($3); }
  ;

designation
  : designator_list '='
  ;

designator_list
  : designator { $$ = new DesignatorListAST(); $$->insertDesignator($1); }
  | designator_list designator { $1->insertDesignator($2); $$ = $1; }
  ;

designator
  : '[' constant_expression ']' { $$ = new ArrayDesignatorAST($2); }
  | '.' IDENTIFIER { $$ = new IdDesignatorAST(*$2); }
  ;

static_assert_declaration
  : STATIC_ASSERT '(' constant_expression ',' STRING_LITERAL ')' ';'
  ;

statement
  : labeled_statement
  | compound_statement
  | expression_statement
  | selection_statement
  | iteration_statement
  | jump_statement
  ;

labeled_statement
  : IDENTIFIER ':' statement
  | CASE constant_expression ':' statement
  | DEFAULT ':' statement
  ;

compound_statement
  : '{' '}' {$$ = new BlockItemListAST();}
  | '{'  block_item_list '}' { $$ = $2; }
  ;

block_item_list
  : block_item { $$ = new BlockItemListAST(); $$->insertBlockItem($1); }
  | block_item_list block_item { $1->insertBlockItem($2); $$ = $1; }
  ;

block_item
  : declaration
  | statement
  ;

expression_statement
  : ';' {$$ = new ExprStmtAST();}
  | expression ';' { $$ = new ExprStmtAST($1); }
  ;

selection_statement
  : IF '(' expression ')' statement ELSE statement { $$ = new IfElseStmtAST($3, $5, $7); }
  | IF '(' expression ')' statement { $$ = new IfElseStmtAST($3, $5); }
  | SWITCH '(' expression ')' statement
  ;

iteration_statement
  : WHILE '(' expression ')' statement { $$ = new WhileStmtAST($3, $5); }
  | DO statement WHILE '(' expression ')' ';'
  | FOR '(' expression_statement expression_statement ')' statement
  | FOR '(' expression_statement expression_statement expression ')' statement
  | FOR '(' declaration expression_statement ')' statement
  | FOR '(' declaration expression_statement expression ')' statement
  ;

jump_statement
  : GOTO IDENTIFIER ';' { $$ = new GotoStmtAST(*$2); }
  | CONTINUE ';'
  | BREAK ';'
  | RETURN ';' { $$ = new ReturnStmtAST(); }
  | RETURN expression ';' { $$ = new ReturnStmtAST($2); }
  ;

translation_unit
  : external_declaration { AST_root = new RootAST(); AST_root->insertExternalUnit($1); }
  | translation_unit external_declaration { AST_root->insertExternalUnit($2); }
  ;

external_declaration
  : function_definition 
  | declaration
  ;

function_definition
  : declaration_specifiers declarator declaration_list compound_statement
  | declaration_specifiers declarator compound_statement { $$ = new FunctionDefinitionAST($1, $2, $3); }
  ;

declaration_list
  : declaration
  | declaration_list declaration
  ;

%%
#include <stdio.h>

void yyerror(const char *s)
{
  fflush(stdout);
  fprintf(stderr, "*** %s\n", s);
}
