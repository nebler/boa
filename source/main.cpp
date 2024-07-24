
#include <algorithm>
#include <cassert>
#include <cctype>
#include <cstdint>
#include <cstdio>
#include <cstdlib>
#include <fstream>
#include <iostream>
#include <map>
#include <memory>
#include <string>
#include <utility>
#include <vector>

#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Type.h>

#include "KaleidoscopeJIT.h"
#include "ast/expressions/expr_ast.h"
#include "context/context-manager.h"
#include "instructions/allocator.h"
#include "lexer/lexer-builder.h"
#include "lexer/tokenizer/token.h"
#include "lexer/tokenizer/tokenizer.h"
#include "llvm/ADT/APFloat.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/PassManager.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Verifier.h"
#include "llvm/Passes/PassBuilder.h"
#include "llvm/Passes/StandardInstrumentations.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Transforms/InstCombine/InstCombine.h"
#include "llvm/Transforms/Scalar.h"
#include "llvm/Transforms/Scalar/GVN.h"
#include "llvm/Transforms/Scalar/Reassociate.h"
#include "llvm/Transforms/Scalar/SimplifyCFG.h"
#include "llvm/Transforms/Utils.h"

using namespace llvm;
using namespace orc;
Value* LogErrorV(const char* Str);
static std::unique_ptr<Tokenizer> tokenizer;

/// BinopPrecedence - This holds the precedence for each binary operator that is
/// defined.
static std::map<char, int> BinopPrecedence;

/// PrototypeAST - This class represents the "prototype" for a function,
/// which captures its name, and its argument names (thus implicitly the number
/// of arguments the function takes).
// A prototype talks aber the external interface for a functionnot the value
// computed by an expression this is also why it does return a llvm value but
// instead returns a llvm function

// Basically, in addition to knowing a name for the prototype, we now keep track
// of whether it was an operator, and if it was, what precedence level the
// operator is at.
class PrototypeAST
{
  std::string Name;
  std::map<std::string, std::string> Args;
  std::string ReturnType;
  bool IsOperator;
  unsigned Precedence;  // Precedence if a binary op.

public:
  PrototypeAST(const std::string& Name,
               std::map<std::string, std::string> Args,
               const std::string& ReturnType,
               bool IsOperator = false,
               unsigned Prec = 0)
      : Name(Name)
      , Args(std::move(Args))
      , IsOperator(IsOperator)
      , Precedence(Prec)
      , ReturnType(ReturnType)
  {
  }
  Function* codegen();
  const std::string& getName() const { return Name; }

  bool isUnaryOp() const { return IsOperator && Args.size() == 1; }
  bool isBinaryOp() const { return IsOperator && Args.size() == 2; }
  const std::string& getReturnType() const { return ReturnType; }
  char getOperatorName() const
  {
    assert(isUnaryOp() || isBinaryOp());
    return Name[Name.size() - 1];
  }

  unsigned getBinaryPrecedence() const { return Precedence; }
};

static std::map<std::string, std::unique_ptr<PrototypeAST>> FunctionProtos;

class StructAST
{
  std::string Name;
  std::map<std::string, std::string> Fields;

public:
  StructAST(const std::string& Name, std::map<std::string, std::string> Fields)
      : Name(Name)
      , Fields(std::move(Fields))
  {
  }
  StructType* codegen();
  const std::string& getName() const { return Name; }
};

/// NumberExprAST - Expression class for numeric literals like "1.0".
class NumberExprAST : public ExprAST
{
  double Val;

public:
  NumberExprAST(double Val)
      : Val(Val)
  {
  }
  // TODO: use a visitor pattern here
  Value* codegen();
};

Value* NumberExprAST::codegen()
{
  // Just creates a constantfp
  return ConstantFP::get(*TheContext, APFloat(Val));
}

/// VariableExprAST - Expression class for referencing a variable, like "a".
class VariableExprAST : public ExprAST
{
  std::string Name;
  // have to implement it
  Value* codegen();

public:
  std::string getName() { return Name; }
  VariableExprAST(const std::string& Name)
      : Name(Name)
  {
  }
};

Value* VariableExprAST::codegen()
{
  // Look this variable up in the function.
  AllocaInst* A = NamedValues[Name];
  if (!A)
    LogErrorV("Unknown variable name");
  // Load the value.
  // Variables live on the stack, so code generating a reference to them
  // actually needs to produce a load from the stack slot:
  return Builder->CreateLoad(A->getAllocatedType(), A, Name.c_str());
}

/// BinaryExprAST - Expression class for a binary operator.
class BinaryExprAST : public ExprAST
{
  char Op;
  std::unique_ptr<ExprAST> LHS, RHS;
  Value* codegen();

public:
  BinaryExprAST(char Op,
                std::unique_ptr<ExprAST> LHS,
                std::unique_ptr<ExprAST> RHS)
      : Op(Op)
      , LHS(std::move(LHS))
      , RHS(std::move(RHS))
  {
  }
};

/// UnaryExprAST - Expression class for a unary operator.
class UnaryExprAST : public ExprAST
{
  char Opcode;
  std::unique_ptr<ExprAST> Operand;

public:
  UnaryExprAST(char Opcode, std::unique_ptr<ExprAST> Operand)
      : Opcode(Opcode)
      , Operand(std::move(Operand))
  {
  }

  Value* codegen() override;
};

/// IfExprAST - Expression class for if/then/else.
class IfExprAST : public ExprAST
{
  std::unique_ptr<ExprAST> Cond, Then, Else;

public:
  IfExprAST(std::unique_ptr<ExprAST> Cond,
            std::unique_ptr<ExprAST> Then,
            std::unique_ptr<ExprAST> Else)
      : Cond(std::move(Cond))
      , Then(std::move(Then))
      , Else(std::move(Else))
  {
  }

  Value* codegen() override;
};

/// VarExprAST - Expression class for var/in
class VarExprAST : public ExprAST
{
  std::vector<std::pair<std::string, std::unique_ptr<ExprAST>>> VarNames;
  std::unique_ptr<ExprAST> Body;

public:
  VarExprAST(
      std::vector<std::pair<std::string, std::unique_ptr<ExprAST>>> VarNames,
      std::unique_ptr<ExprAST> Body)
      : VarNames(std::move(VarNames))
      , Body(std::move(Body))
  {
  }

  Value* codegen() override;
};

Value* VarExprAST::codegen()
{
  std::vector<AllocaInst*> OldBindings;

  Function* TheFunction = Builder->GetInsertBlock()->getParent();

  // Register all variables and emit their initializer.
  for (unsigned i = 0, e = VarNames.size(); i != e; ++i) {
    const std::string& VarName = VarNames[i].first;
    ExprAST* Init = VarNames[i].second.get();
    // Emit the initializer before adding the variable to scope, this prevents
    // the initializer from referencing the variable itself, and permits stuff
    // like this:
    //  var a = 1 in
    //    var a = a in ...   # refers to outer 'a'.
    Value* InitVal;
    if (Init) {
      InitVal = Init->codegen();
      if (!InitVal)
        return nullptr;
    } else {  // If not specified, use 0.0.
      InitVal = ConstantFP::get(*TheContext, APFloat(0.0));
    }

    AllocaInst* Alloca = CreateEntryBlockAlloca(TheFunction, VarName);
    Builder->CreateStore(InitVal, Alloca);

    // Remember the old variable binding so that we can restore the binding when
    // we unrecurse.
    OldBindings.push_back(NamedValues[VarName]);

    // Remember this binding.
    NamedValues[VarName] = Alloca;
  }
  // Codegen the body, now that all vars are in scope.
  Value* BodyVal = Body->codegen();
  if (!BodyVal)
    return nullptr;
  // Pop all our variables from scope.
  for (unsigned i = 0, e = VarNames.size(); i != e; ++i)
    NamedValues[VarNames[i].first] = OldBindings[i];

  // Return the body computation.
  return BodyVal;
}

/// ForExprAST - Expression class for for/in.
class ForExprAST : public ExprAST
{
  std::string VarName;
  std::unique_ptr<ExprAST> Start, End, Step, Body;

public:
  ForExprAST(const std::string& VarName,
             std::unique_ptr<ExprAST> Start,
             std::unique_ptr<ExprAST> End,
             std::unique_ptr<ExprAST> Step,
             std::unique_ptr<ExprAST> Body)
      : VarName(VarName)
      , Start(std::move(Start))
      , End(std::move(End))
      , Step(std::move(Step))
      , Body(std::move(Body))
  {
  }

  Value* codegen() override;
};

Value* IfExprAST::codegen()
{
  Value* CondV = Cond->codegen();
  if (!CondV)
    return nullptr;

  // Convert condition to a bool by comparing non-equal to 0.0.
  // we don't have actual bools just 0 and 1
  CondV = Builder->CreateFCmpONE(
      CondV, ConstantFP::get(*TheContext, APFloat(0.0)), "ifcond");

  Function* TheFunction = Builder->GetInsertBlock()->getParent();

  // Create blocks for the then and else cases.  Insert the 'then' block at the
  // end of the function.
  BasicBlock* ThenBB = BasicBlock::Create(*TheContext, "then", TheFunction);
  BasicBlock* ElseBB = BasicBlock::Create(*TheContext, "else");
  BasicBlock* MergeBB = BasicBlock::Create(*TheContext, "ifcont");

  Builder->CreateCondBr(CondV, ThenBB, ElseBB);

  // Emit then value.
  Builder->SetInsertPoint(ThenBB);

  Value* ThenV = Then->codegen();
  if (!ThenV)
    return nullptr;

  Builder->CreateBr(MergeBB);
  // Codegen of 'Then' can change the current block, update ThenBB for the PHI.
  // What is an InsertBlock?
  ThenBB = Builder->GetInsertBlock();

  // Emit else block.
  TheFunction->insert(TheFunction->end(), ElseBB);
  Builder->SetInsertPoint(ElseBB);

  Value* ElseV = Else->codegen();
  if (!ElseV)
    return nullptr;

  Builder->CreateBr(MergeBB);
  // codegen of 'Else' can change the current block, update ElseBB for the PHI.
  ElseBB = Builder->GetInsertBlock();

  // Emit merge block.
  TheFunction->insert(TheFunction->end(), MergeBB);
  Builder->SetInsertPoint(MergeBB);
  PHINode* PN = Builder->CreatePHI(Type::getDoubleTy(*TheContext), 2, "iftmp");

  PN->addIncoming(ThenV, ThenBB);
  PN->addIncoming(ElseV, ElseBB);
  return PN;
}

Function* getFunction(std::string Name)
{
  // First, see if the function has already been added to the current module.
  if (auto* F = TheModule->getFunction(Name)) {
    std::cout << "Module foofer" << std::endl;
    return F;
  }

  if (auto* F = TheModule->getFunction(Name + "_ctor")) {
    std::cout << "Module foofer" << std::endl;
    return F;
  }

  // If not, check whether we can codegen the declaration from some existing
  // prototype.
  auto FI = FunctionProtos.find(Name);
  if (FI != FunctionProtos.end())
    return FI->second->codegen();

  // If no existing prototype exists, return null.
  return nullptr;
}

Value* BinaryExprAST::codegen()
{
  // Special case '=' because we don't want to emit the LHS as an expression.
  // It does not follow the emit LHS, emit RHS do computiation model
  if (Op == '=') {
    // Assignment requires the LHS to be an identifier.
    // This assume we're building without RTTI because LLVM builds that way by
    // default.  If you build LLVM with RTTI this can be changed to a
    // dynamic_cast for automatic error checking.
    VariableExprAST* LHSE = static_cast<VariableExprAST*>(LHS.get());
    if (!LHSE)
      return LogErrorV("destination of '=' must be a variable");
    // Codegen the RHS.
    Value* Val = RHS->codegen();
    if (!Val)
      return nullptr;

    // Look up the name.
    Value* Variable = NamedValues[LHSE->getName()];
    if (!Variable)
      return LogErrorV("Unknown variable name");

    Builder->CreateStore(Val, Variable);
    return Val;
  }
  // we generate code for the right as well as the left hand side
  Value* L = LHS->codegen();
  Value* R = RHS->codegen();
  if (!L || !R) {
    return nullptr;
  }
  switch (Op) {
    case '+':
      return Builder->CreateFAdd(L, R, "addtmp");
    case '-':
      return Builder->CreateFSub(L, R, "subtmp");
    case '*':
      return Builder->CreateFMul(L, R, "multmp");
    case '<':
      // TODO: understand this
      L = Builder->CreateFCmpULT(L, R, "cmptmp");
      // Convert bool 0/1 to double 0.0 or 1.0
      return Builder->CreateUIToFP(
          L, Type::getDoubleTy(*TheContext), "booltmp");
    default:
      break;
  }

  // If it wasn't a builtin binary operator, it must be a user defined one. Emit
  // a call to it.
  Function* F = getFunction(std::string("binary") + Op);
  assert(F && "binary operator not found!");

  Value* Ops[2] = {L, R};
  return Builder->CreateCall(F, Ops, "binop");
}

/// CallExprAST - Expression class for function calls.
class CallExprAST : public ExprAST
{
  std::string Callee;
  std::vector<std::unique_ptr<ExprAST>> Args;
  Value* codegen();

public:
  CallExprAST(const std::string& Callee,
              std::vector<std::unique_ptr<ExprAST>> Args)
      : Callee(Callee)
      , Args(std::move(Args))
  {
  }
};

Value* CallExprAST::codegen()
{
  // Look up the name in the global module table.
  Function* CalleeF = getFunction(Callee);
  if (!CalleeF) {
    return LogErrorV("Unknown function refeenced");
  }

  // if there is argument mismatch error
  if (CalleeF->arg_size() != Args.size()) {
    return LogErrorV("Incorrec # arguments passed");
  }

  std::vector<Value*> ArgsV;
  for (unsigned i = 0, e = Args.size(); i != e; i++) {
    // for every argument generate the code
    ArgsV.push_back(Args[i]->codegen());
    if (!ArgsV.back()) {
      return nullptr;
    }
  }
  return Builder->CreateCall(CalleeF, ArgsV, "calltmp");
}

Function* PrototypeAST::codegen()
{
  // Make the function type:  double(double,double) etc.
  std::vector<Type*> ArgumentTypes;
  for (auto const& x : this->Args) {
    ArgumentTypes.push_back(DefinedTypes[x.second]);
  }
  // The call to FunctionType::get creates the FunctionType that should be used
  // for a given Prototype. Since all function arguments in Kaleidoscope are of
  // type double, the first line creates a vector of “N” LLVM double types. It
  // then uses the Functiontype::get method to create a function type that takes
  // “N” doubles as arguments, returns one double as a result, and that is not
  // vararg (the false parameter indicates this). Note that Types in LLVM are
  // uniqued just like Constants are, so you don’t “new” a type, you “get” it.
  Type* type = DefinedTypes[this->ReturnType];
  FunctionType* FT = FunctionType::get(type, ArgumentTypes, false);
  // creates the IR function corresponding to the Prototype
  Function* F =
      Function::Create(FT, Function::ExternalLinkage, Name, TheModule.get());

  // Set names for all arguments from function from above
  unsigned Idx = 0;
  auto it = this->Args.begin();
  for (auto& Arg : F->args()) {
    std::advance(it, Idx++);
    Arg.setName(it->first);
  }

  return F;
}

// Output for-loop as:
//   ...
//   start = startexpr
//   goto loop
// loop:
//   variable = phi [start, loopheader], [nextvariable, loopend]
//   ...
//   bodyexpr
//   ...
// loopend:
//   step = stepexpr
//   nextvariable = variable + step
//   endcond = endexpr
//   br endcond, loop, endloop
// outloop:

// For each argument, we make an alloca, store the input value to the function
// into the alloca, and register the alloca as the memory location for the
// argument.
Value* ForExprAST::codegen()
{
  Function* TheFunction = Builder->GetInsertBlock()->getParent();

  // Create an alloca for the variable in the entry block.
  AllocaInst* Alloca = CreateEntryBlockAlloca(TheFunction, VarName);

  // Emit the start code first, without 'variable' in scope.
  Value* StartVal = Start->codegen();
  if (!StartVal)
    return nullptr;

  // Store the value into the alloca.
  Builder->CreateStore(StartVal, Alloca);

  // Make the new basic block for the loop header, inserting after current
  // block.
  BasicBlock* LoopBB = BasicBlock::Create(*TheContext, "loop", TheFunction);

  // Insert an explicit fall through from the current block to the LoopBB.
  Builder->CreateBr(LoopBB);

  // Start insertion in LoopBB.
  Builder->SetInsertPoint(LoopBB);

  // Within the loop, the variable is defined equal to the PHI node.  If it
  // shadows an existing variable, we have to restore it, so save it now.
  AllocaInst* OldVal = NamedValues[VarName];
  NamedValues[VarName] = Alloca;

  // Emit the body of the loop.  This, like any other expr, can change the
  // current BB.  Note that we ignore the value computed by the body, but don't
  // allow an error.
  if (!Body->codegen())
    return nullptr;

  // Emit the step value.
  Value* StepVal = nullptr;
  if (Step) {
    StepVal = Step->codegen();
    if (!StepVal)
      return nullptr;
  } else {
    // If not specified, use 1.0.
    StepVal = ConstantFP::get(*TheContext, APFloat(1.0));
  }

  // Compute the end condition.
  Value* EndCond = End->codegen();
  if (!EndCond)
    return nullptr;

  // Reload, increment, and restore the alloca.  This handles the case where
  // the body of the loop mutates the variable.
  Value* CurVar =
      Builder->CreateLoad(Alloca->getAllocatedType(), Alloca, VarName.c_str());
  Value* NextVar = Builder->CreateFAdd(CurVar, StepVal, "nextvar");
  Builder->CreateStore(NextVar, Alloca);

  // Convert condition to a bool by comparing non-equal to 0.0.
  EndCond = Builder->CreateFCmpONE(
      EndCond, ConstantFP::get(*TheContext, APFloat(0.0)), "loopcond");

  // Create the "after loop" block and insert it.
  BasicBlock* AfterBB =
      BasicBlock::Create(*TheContext, "afterloop", TheFunction);

  // Insert the conditional branch into the end of LoopEndBB.
  Builder->CreateCondBr(EndCond, LoopBB, AfterBB);

  // Any new code will be inserted in AfterBB.
  Builder->SetInsertPoint(AfterBB);

  // Restore the unshadowed variable.
  if (OldVal)
    NamedValues[VarName] = OldVal;
  else
    NamedValues.erase(VarName);

  // for expr always returns 0.0.
  return Constant::getNullValue(Type::getDoubleTy(*TheContext));
}

/// FunctionAST - This class represents a function definition itself.
class FunctionAST
{
  std::unique_ptr<PrototypeAST> Proto;
  std::unique_ptr<ExprAST> Body;

public:
  Function* codegen();
  FunctionAST(std::unique_ptr<PrototypeAST> Proto,
              std::unique_ptr<ExprAST> Body)
      : Proto(std::move(Proto))
      , Body(std::move(Body))
  {
  }
};

Value* UnaryExprAST::codegen()
{
  Value* OperandV = Operand->codegen();
  if (!OperandV)
    return nullptr;

  Function* F = getFunction(std::string("unary") + Opcode);
  if (!F)
    return LogErrorV("Unknown unary operator");

  return Builder->CreateCall(F, OperandV, "unop");
}

Function* FunctionAST::codegen()
{
  // Transfer ownership of the prototype to the FunctionProtos map, but keep a
  // reference to it for use below.
  auto& P = *Proto;
  FunctionProtos[Proto->getName()] = std::move(Proto);
  Function* TheFunction = getFunction(P.getName());
  if (!TheFunction)
    return nullptr;

  // If this is an operator, install it.
  if (P.isBinaryOp())
    BinopPrecedence[P.getOperatorName()] = P.getBinaryPrecedence();
  // Now we get to the point where the Builder is set up. The first line creates
  // a new basic block (named “entry”), which is inserted into TheFunction.
  BasicBlock* BB = BasicBlock::Create(*TheContext, "entry", TheFunction);
  Builder->SetInsertPoint(BB);
  NamedValues.clear();
  for (auto& Arg : TheFunction->args()) {
    // Create an alloca for this variable.
    AllocaInst* Alloca =
        CreateEntryBlockAlloca(TheFunction, std::string(Arg.getName()));

    // Store the initial value into the alloca.
    Builder->CreateStore(&Arg, Alloca);

    // Add arguments to variable symbol table.
    NamedValues[std::string(Arg.getName())] = Alloca;
  }
  if (Value* RetVal = Body->codegen()) {
    /*
    ret <type> <value>       ; Return a value from a non-void function
    ret void                 ; Return from void function

    The ‘ret’ instruction is used to return control flow (and optionally a
    value) from a function back to the caller. There are two forms of the ‘ret’
    instruction: one that returns a value and then causes control flow, and one
    that just causes control flow to occur.
    */
    Builder->CreateRet(RetVal);
    // Validate the generated code, checking for consistency.
    verifyFunction(*TheFunction);
    // Optimize the function.
    TheFPM->run(*TheFunction, *TheFAM);
    return TheFunction;
  }

  // Error reading body, remove function.
  TheFunction->eraseFromParent();
  return nullptr;
}

/// LogError* - These are little helper functions for error handling.
std::unique_ptr<ExprAST> LogError(const char* Str)
{
  fprintf(stderr, "Error: %s\n", Str);
  return nullptr;
}
std::unique_ptr<PrototypeAST> LogErrorP(const char* Str)
{
  LogError(Str);
  return nullptr;
}

Value* LogErrorV(const char* Str)
{
  LogError(Str);
  return nullptr;
}

static std::unique_ptr<ExprAST> ParseExpression();

/// numberexpr ::= number
static std::unique_ptr<ExprAST> ParseNumberExpr()
{
  auto Result = std::make_unique<NumberExprAST>(NumVal);
  tokenizer->getNextToken();  // consume the number
  return std::move(Result);
}

/// parenexpr ::= '(' expression ')'
static std::unique_ptr<ExprAST> ParseParenExpr()
{
  tokenizer->getNextToken();  // eat (.
  auto V = ParseExpression();
  if (!V)
    return nullptr;

  if (CurTok != ')')
    return LogError("expected ')'");
  tokenizer->getNextToken();  // eat ).
  return V;
}

/// identifierexpr
///   ::= identifier
///   ::= identifier '(' expression* ')'
static std::unique_ptr<ExprAST> ParseIdentifierExpr()
{
  std::string IdName = IdentifierStr;

  tokenizer->getNextToken();  // eat identifier.
                              // Simple variable ref.
  if (CurTok != '(') {
    return std::make_unique<VariableExprAST>(IdName);
  }

  // Call.
  tokenizer->getNextToken();  // eat (
  std::vector<std::unique_ptr<ExprAST>> Args;
  if (CurTok != ')') {
    while (true) {
      if (auto Arg = ParseExpression())
        Args.push_back(std::move(Arg));
      else
        return nullptr;

      if (CurTok == ')')
        break;

      if (CurTok != ',')
        return LogError("Expected ')' or ',' in argument list");
      tokenizer->getNextToken();
    }
  }

  // Eat the ')'.
  tokenizer->getNextToken();

  return std::make_unique<CallExprAST>(IdName, std::move(Args));
}

/// ifexpr ::= 'if' expression 'then' expression 'else' expression
static std::unique_ptr<ExprAST> ParseIfExpr()
{
  tokenizer->getNextToken();  // eat the if.

  // condition.
  auto Cond = ParseExpression();
  if (!Cond)
    return nullptr;

  if (CurTok != tok_then)
    return LogError("expected then");
  tokenizer->getNextToken();  // eat the then

  auto Then = ParseExpression();
  if (!Then)
    return nullptr;

  if (CurTok != tok_else)
    return LogError("expected else");

  tokenizer->getNextToken();

  auto Else = ParseExpression();
  if (!Else)
    return nullptr;

  return std::make_unique<IfExprAST>(
      std::move(Cond), std::move(Then), std::move(Else));
}

/// forexpr ::= 'for' identifier '=' expr ',' expr (',' expr)? 'in' expression
static std::unique_ptr<ExprAST> ParseForExpr()
{
  tokenizer->getNextToken();  // eat the for.

  if (CurTok != tok_identifier)
    return LogError("expected identifier after for");

  std::string IdName = IdentifierStr;
  tokenizer->getNextToken();  // eat identifier.

  if (CurTok != '=')
    return LogError("expected '=' after for");
  tokenizer->getNextToken();  // eat '='.

  auto Start = ParseExpression();
  if (!Start)
    return nullptr;
  if (CurTok != ',')
    return LogError("expected ',' after for start value");
  tokenizer->getNextToken();

  auto End = ParseExpression();
  if (!End)
    return nullptr;

  // The step value is optional.
  std::unique_ptr<ExprAST> Step;
  if (CurTok == ',') {
    tokenizer->getNextToken();
    Step = ParseExpression();
    if (!Step)
      return nullptr;
  }

  if (CurTok != tok_in)
    return LogError("expected 'in' after for");
  tokenizer->getNextToken();  // eat 'in'.

  auto Body = ParseExpression();
  if (!Body)
    return nullptr;

  return std::make_unique<ForExprAST>(IdName,
                                      std::move(Start),
                                      std::move(End),
                                      std::move(Step),
                                      std::move(Body));
}

// varexpr ::= 'var' identifier ('=' expression)?
//                    (',' identifier ('=' expression)?)* 'in' expression
static std::unique_ptr<ExprAST> ParseVarExpr()
{
  tokenizer->getNextToken();  // eat the var.

  std::vector<std::pair<std::string, std::unique_ptr<ExprAST>>> VarNames;
  // At least one variable name is required.
  // LHS has to be an identifier
  if (CurTok != tok_identifier)
    return LogError("expected identifier after var");

  // this is a loop because we allow multiple variable definitions at once
  // a, b, c = 4 + 5
  while (true) {
    std::string Name = IdentifierStr;
    tokenizer->getNextToken();  // eat identifier.

    // Read the optional initializer.
    std::unique_ptr<ExprAST> Init;
    if (CurTok == '=') {
      tokenizer->getNextToken();  // eat the '='.

      Init = ParseExpression();
      if (!Init)
        return nullptr;
    }

    VarNames.push_back(std::make_pair(Name, std::move(Init)));

    // End of var list, exit loop.
    if (CurTok != ',')
      break;
    tokenizer->getNextToken();  // eat the ','.

    if (CurTok != tok_identifier)
      return LogError("expected identifier list after var");
  }
  if (CurTok != tok_in)
    return LogError("expected 'in' keyword after 'var'");
  tokenizer->getNextToken();  // eat 'in'.

  auto Body = ParseExpression();
  if (!Body)
    return nullptr;

  return std::make_unique<VarExprAST>(std::move(VarNames), std::move(Body));
}

/// primary
///   ::= identifierexpr
///   ::= numberexpr
///   ::= parenexpr
static std::unique_ptr<ExprAST> ParsePrimary()
{
  switch (CurTok) {
    default:
      return LogError("unknown token when expecting an expression");
    case tok_identifier:
      return ParseIdentifierExpr();
    case tok_number:
      return ParseNumberExpr();
    case '(':
      return ParseParenExpr();
    case tok_if:
      return ParseIfExpr();
    case tok_for:
      return ParseForExpr();
    case tok_var:
      return ParseVarExpr();
  }
}

/// unary
///   ::= primary
///   ::= '!' unary
static std::unique_ptr<ExprAST> ParseUnary()
{
  // If the current token is not an operator, it must be a primary expr.
  if (!isascii(CurTok) || CurTok == '(' || CurTok == ',')
    return ParsePrimary();

  // If this is a unary operator, read it.
  int Opc = CurTok;
  tokenizer->getNextToken();
  if (auto Operand = ParseUnary())
    return std::make_unique<UnaryExprAST>(Opc, std::move(Operand));
  return nullptr;
}

/// binoprhs
///   ::= ('+' primary)*
static std::unique_ptr<ExprAST> ParseBinOpRHS(int ExprPrec,
                                              std::unique_ptr<ExprAST> LHS)
{
  // If this is a binop, find its precedence.
  while (true) {
    int TokPrec = tokenizer->GetTokPrecedence(BinopPrecedence);

    // If this is a binop that binds at least as tightly as the current binop,
    // consume it, otherwise we are done.
    if (TokPrec < ExprPrec)
      return LHS;

    // Okay, we know this is a binop.
    int BinOp = CurTok;
    tokenizer->getNextToken();  // eat binop

    // Parse the unary expression after the binary operator.
    auto RHS = ParseUnary();
    if (!RHS)
      return nullptr;
    // If BinOp binds less tightly with RHS than the operator after RHS, let
    // the pending operator take RHS as its LHS.
    int NextPrec = tokenizer->GetTokPrecedence(BinopPrecedence);
    if (TokPrec < NextPrec) {
      RHS = ParseBinOpRHS(TokPrec + 1, std::move(RHS));
      if (!RHS)
        return nullptr;
    }

    // Merge LHS/RHS.
    LHS =
        std::make_unique<BinaryExprAST>(BinOp, std::move(LHS), std::move(RHS));
  }
}

/// expression
///   ::= unary binoprhs
///
static std::unique_ptr<ExprAST> ParseExpression()
{
  auto LHS = ParseUnary();
  if (!LHS)
    return nullptr;

  return ParseBinOpRHS(0, std::move(LHS));
}

static std::map<std::string, llvm::StructType*> DefinedStructs;
static std::map<std::string, llvm::Type*> DefinedTypes;
static void PopulateTypes()
{
  DefinedTypes["int"] = llvm::Type::getInt32Ty(*TheContext);
  DefinedTypes["float"] = llvm::Type::getFloatTy(*TheContext);
}

// Dummy CheckType function to demonstrate the functionality
std::string CheckType()
{
  int tok = tokenizer->getNextToken();
  if (tok == tok_int) {
    return "int";
  } else if (tok == tok_float) {
    return "float";
  }
  auto it = DefinedTypes[IdentifierStr];
  if (it != nullptr) {
    return IdentifierStr;
  } else {
    std::cerr << "Couldn't find type of " << IdentifierStr << std::endl;
    return nullptr;
  }
}

/*
struct User {
    active bool,
    username String,
    email String,
    sign_in_count u64,
    foo Another
}
*/
// Function to parse the struct and create LLVM IR
static std::unique_ptr<StructAST> ParseStruct()
{
  // struct has already been parsed before
  tokenizer->getNextToken();  // get the struct name

  string name = IdentifierStr;
  tokenizer->getNextToken();  // eat the '{'

  std::map<string, string> structMembers;

  tokenizer->getNextToken();

  if (CurTok == '}') {
    return make_unique<StructAST>(StructAST(name, structMembers));
  }

  while (true) {
    // Parse the member type and name, assume we have a function `CheckType`
    // that returns an LLVM type
    // Parse the member name

    if (CurTok != tok_identifier)
    {  // Assuming TokenIdentifier is the token type for identifiers
      std::cerr << "Error: expected member name!" << std::endl;
      return nullptr;
    }
    std::string memberName = IdentifierStr;

    std::string memberType = CheckType();
    structMembers[memberName] = memberType;
    tokenizer->getNextToken();
    if (CurTok != ',') {
      break;
    } else {
      tokenizer->getNextToken();
    }
  }

  if (CurTok == '}') {
    return make_unique<StructAST>(StructAST(name, structMembers));
  } else {
    std::cerr << "Something went wrong I was expecting a }" << std::endl;
  }
}

StructType* StructAST::codegen()
{
  llvm::StructType* structType =
      StructType::create(*TheContext, llvm::StringRef(this->getName()));
  std::vector<llvm::Type*> memberTypes;
  for (auto it = this->Fields.begin(); it != this->Fields.end(); ++it) {
    memberTypes.push_back(DefinedTypes[it->first]);
  }
  structType->setBody(memberTypes);
  DefinedTypes[this->getName()] = structType;
  tokenizer->getNextToken();

  // Create a constructor for the structs
  llvm::FunctionType* FuncType =
      llvm::FunctionType::get(structType, {memberTypes}, false);
  llvm::Function* TheFunction =
      llvm::Function::Create(FuncType,
                             llvm::Function::ExternalLinkage,
                             this->getName() + "_ctor",
                             TheModule.get());
  llvm::BasicBlock* entry =
      llvm::BasicBlock::Create(*TheContext, "entry", TheFunction);
  Builder->SetInsertPoint(entry);

  if (structType) {
    llvm::Value* structAlloc = Builder->CreateAlloca(structType);

    // Initialize struct members
    unsigned idx = 0;
    for (llvm::Function::arg_iterator arg = TheFunction->arg_begin();
         arg != TheFunction->arg_end();
         ++arg, ++idx)
    {
      llvm::Value* memberPtr =
          Builder->CreateStructGEP(structType, structAlloc, idx);
      Builder->CreateStore(arg, memberPtr);
    }

    // Load the struct and return it
    llvm::Value* loadedStruct = Builder->CreateLoad(structType, structAlloc);
    Builder->CreateRet(loadedStruct);
  } else {
    std::cerr << "Error: structType is null." << std::endl;
    return nullptr;  // Or handle the error appropriately
  }
  auto constructorPrototytpe =
      PrototypeAST(this->getName() + "_ctor", this->Fields, this->getName());
  FunctionProtos[this->getName()] =
      make_unique<PrototypeAST>(constructorPrototytpe);
  return structType;
}

/// prototype
///   ::= id '(' id* ')'
static std::unique_ptr<PrototypeAST> ParsePrototype()
{
  std::string FnName;
  unsigned Kind = 0;  // 0 = identifier, 1 = unary, 2 = binary.
  unsigned BinaryPrecedence = 30;

  switch (CurTok) {
    default:
      return LogErrorP("Expected function name in prototype");
    case tok_identifier:
      FnName = IdentifierStr;
      // then this can have a different type then double
      // can be a function or a variable
      Kind = 0;
      tokenizer->getNextToken();
      break;
    case tok_unary:
      tokenizer->getNextToken();
      if (!isascii(CurTok))
        return LogErrorP("Expected unary operator");
      FnName = "unary";
      FnName += (char)CurTok;
      Kind = 1;
      tokenizer->getNextToken();
      break;
    case tok_binary:
      tokenizer->getNextToken();
      if (!isascii(CurTok))
        return LogErrorP("Expected binary operator");
      FnName = "binary";
      FnName += (char)CurTok;
      // type of double (for now)
      Kind = 2;
      tokenizer->getNextToken();

      // Read the precedence if present.
      if (CurTok == tok_number) {
        if (NumVal < 1 || NumVal > 100)
          return LogErrorP("Invalid precedence: must be 1..100");
        BinaryPrecedence = (unsigned)NumVal;
        tokenizer->getNextToken();
      }
      break;
  }
  if (CurTok != '(')
    return LogErrorP("Expected '(' in prototype");

  std::vector<std::string> ArgNames;
  while (tokenizer->getNextToken() == tok_identifier)

    ArgNames.push_back(IdentifierStr);
  if (CurTok != ')')
    return LogErrorP("Expected ')' in prototype");

  // success.
  tokenizer->getNextToken();  // eat ')'.

  // Verify right number of names for operator.
  if (Kind && ArgNames.size() != Kind)
    return LogErrorP("Invalid number of operands for operator");

  return std::make_unique<PrototypeAST>(
      FnName, std::move(ArgNames), Kind != 0, BinaryPrecedence);
}

/// definition ::= 'def' prototype expression
static std::unique_ptr<FunctionAST> ParseDefinition()
{
  tokenizer->getNextToken();  // eat fn.
  std::unique_ptr<PrototypeAST> Proto = ParsePrototype();
  if (!Proto)
    return nullptr;

  if (auto E = ParseExpression())
    return std::make_unique<FunctionAST>(std::move(Proto), std::move(E));
  return nullptr;
}

/// toplevelexpr ::= expression
static std::unique_ptr<FunctionAST> ParseTopLevelExpr()
{
  if (auto E = ParseExpression()) {
    printf("parsing top level expression \n");
    // Make an anonymous proto.
    auto Proto = std::make_unique<PrototypeAST>("__anon_expr",
                                                std::vector<std::string>());
    return std::make_unique<FunctionAST>(std::move(Proto), std::move(E));
  }
  return nullptr;
}

/// external ::= 'extern' prototype
static std::unique_ptr<PrototypeAST> ParseExtern()
{
  tokenizer->getNextToken();  // eat extern.
  return ParsePrototype();
}

static void HandleFunction()
{
  if (auto FnAST = ParseDefinition()) {
    if (auto* FnIR = FnAST->codegen()) {
      fprintf(stderr, "Read function definition:");
      FnIR->print(errs());
      fprintf(stderr, "\n");
      ExitOnErr(TheJIT->addModule(
          ThreadSafeModule(std::move(TheModule), std::move(TheContext))));
      InitializeModuleAndManagers();
    }
  } else {
    // Skip token for error recovery.
    tokenizer->getNextToken();
  }
}

static void HandleExtern()
{
  if (auto ProtoAST = ParseExtern()) {
    if (auto* FnIR = ProtoAST->codegen()) {
      fprintf(stderr, "Read extern: ");
      FnIR->print(errs());
      fprintf(stderr, "\n");
      FunctionProtos[ProtoAST->getName()] = std::move(ProtoAST);
    } else {
      printf("no codegen");
    }
  } else {
    printf("no extern");
    // Skip token for error recovery.
    tokenizer->getNextToken();
  }
}

static void HandleStruct()
{
  auto structAST = ParseStruct();
  structAST->codegen();
}

#ifdef _WIN32
#  define DLLEXPORT __declspec(dllexport)
#else
#  define DLLEXPORT
#endif

/// putchard - putchar that takes a double and returns 0.
extern "C" DLLEXPORT double putchard(double X)
{
  fputc((char)X, stderr);
  return 0;
}

/// printd - printf that takes a double prints it as "%f\n", returning 0.
extern "C" DLLEXPORT double printd(double X)
{
  fprintf(stderr, "%f \n", X);
  return 0;
}

static void HandleTopLevelExpression()
{
  // Evaluate a top-level expression into an anonymous function.
  if (auto FnAST = ParseTopLevelExpr()) {
    if (auto* FnIR = FnAST->codegen()) {
      fprintf(stderr, "Read top-level expression: \n");
      FnIR->print(errs());
      fprintf(stderr, "\n");
      // Create a ResourceTracker to track JIT'd memory allocated to our
      // anonymous expression -- that way we can free it after executing.
      auto RT = TheJIT->getMainJITDylib().createResourceTracker();

      auto TSM = ThreadSafeModule(std::move(TheModule), std::move(TheContext));
      ExitOnErr(TheJIT->addModule(std::move(TSM), RT));
      // todo: why do we need to call this again?
      // from docs: Once the module has been added to the JIT it can no longer
      // be modified,
      //  so we also open a new module to hold subsequent code by calling
      //  InitializeModuleAndPassManager().
      InitializeModuleAndManagers();
      // Search the JIT for the __anon_expr symbol.
      auto ExprSymbol = ExitOnErr(TheJIT->lookup("__anon_expr"));
      assert(ExprSymbol.getAddress().getValue() != 0 && "Function not found");
      // Get the symbol's address and cast it to the right type (takes no
      // arguments, returns a double) so we can call it as a native function.
      double (*FP)() = ExprSymbol.getAddress().toPtr<double (*)()>();
      fprintf(stderr, "Evaluated to %f\n", FP());
      // Delete the anonymous expression module from the JIT.
      ExitOnErr(RT->remove());
    }
  } else {
    // Skip token for error recovery.
    tokenizer->getNextToken();
  }
}

/// top ::= definition | external | expression | ';'
static void MainLoop()
{
  while (true) {
    fprintf(stderr, "ready> ");
    switch (CurTok) {
      case tok_eof:
        return;
      case ';':  // ignore top-level semicolons.
        tokenizer->getNextToken();
        break;
      case tok_fn:
        HandleFunction();
        break;
      case tok_extern:
        HandleExtern();
        break;
      case tok_strcut:
        std::cout << "struct" << std::endl;
        HandleStruct();
        break;
      default:
        HandleTopLevelExpression();
        break;
    }
  }
}

//===----------------------------------------------------------------------===//
// Main driver code.
//===----------------------------------------------------------------------===//

int main(int argc, char* argv[])
{
  tokenizer = std::make_unique<Tokenizer>(createTokenizer(argv));
  initalizeNativeTarget();
  setUpJIT();
  InitializeModuleAndManagers();
  // Install standard binary operators.
  // 1 is lowest precedence.
  BinopPrecedence['='] = 2;
  BinopPrecedence['<'] = 10;
  BinopPrecedence['+'] = 20;
  BinopPrecedence['-'] = 20;
  BinopPrecedence['*'] = 40;  // highest.
  // Prime the first token.
  PopulateTypes();
  fprintf(stderr, "ready> ");
  tokenizer->getNextToken();
  // Make the module, which holds all the code.
  // todo: check the Create Method and understand the expected type
  // is it similar to rust type?
  // Run the main "interpreter loop" now.
  MainLoop();

  // Print out all of the generated code.
  TheModule->print(errs(), nullptr);

  return 0;
}