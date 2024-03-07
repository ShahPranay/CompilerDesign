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

// Binary expression AST node
class BinaryExprAST : public ExprAST {
    char op;
    std::unique_ptr<ExprAST> left, right;

public:
    BinaryExprAST(char op, std::unique_ptr<ExprAST> LHS, std::unique_ptr<ExprAST> RHS)
        : op(op), left(std::move(LHS)), right(std::move(RHS)) {}
};

// Prototype AST node
class PrototypeAST : public NodeAST {
    std::string name;
    std::vector<std::string> args;

public:
    PrototypeAST(const std::string &Name, std::vector<std::string> &Args)
        : name(Name), args(Args) {}

    const std::string &getName() const { return name; }
};

class BlockStmtAST : public StmtAST {
    std::vector<std::unique_ptr<StmtAST>> statements;
};

// Function AST Node
class FunctionAST : public NodeAST {
    std::unique_ptr<PrototypeAST> prototype;
    std::unique_ptr<BlockStmtAST> body;

public:
    FunctionAST(std::unique_ptr<PrototypeAST> Prototype, std::unique_ptr<BlockStmtAST> Body)
        : prototype(std::move(Prototype)), body(std::move(Body)) {} 
};

