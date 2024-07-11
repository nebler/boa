#ifndef EXPR_AST_H
#define EXPR_AST_H
using namespace llvm;
class ExprAST
{
public:
    virtual ~ExprAST() = default;
    // C++ syntax =0 means no default implementation and subclasses have to implement it.
    // Value represents a SSA
    virtual llvm::Value *codegen() = 0;
};

#endif