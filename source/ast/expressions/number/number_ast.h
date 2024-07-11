#ifndef NUMBER_AST_H
#define NUMBER_AST_H

/// NumberExprAST - Expression class for numeric literals like "1.0".
class NumberExprAST : public ExprAST
{
    double Val;

public:
    NumberExprAST(double Val) : Val(Val) {}
    // TODO: use a visitor pattern here
    Value *codegen();
};
