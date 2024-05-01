#include <string>
#include <iostream>
#include <map>
using namespace std;

enum Token
{
    tok_eof = -1,
    tok_def = -2,
    tok_extern = -3,
    tok_identifier = -4,
    tok_number = -5,
};

static std::string IdentifierStr; //
static double NumVal;

static int gettok()
{

    // LastChar is a local variable with "static" storage duration. It behaves like a global variable with limited scope.
    // Therefore, the value in LastChar is saved between function calls.
    // The gettok procedure saves the next character that needs to be processed in LastChar.The initial character is space.
    // This doesn't matter since whitespaces are anyway ignored in the language. static int LastChar = ' ';
    static int LastChar = ' ';
    while (isspace(LastChar))
    {
        // getChar is a standard C library function and reads the input
        // reading what I am typing in.
        // just reads one
        LastChar = getchar();
    }

    // identifier starts with a letter or with a number. Get Identifiert
    if (isalpha(LastChar))
    {
        IdentifierStr = LastChar;
        while (isalnum((LastChar = getchar())))
        {
            IdentifierStr += LastChar;
        }
        cout << IdentifierStr;
        if (IdentifierStr == "def")
        {
            return tok_def;
        }
        if (IdentifierStr == "extern")
        {
            return tok_extern;
        }
        return tok_identifier;
    }

    if (isdigit(LastChar) || LastChar == '.')
    {
        std::string NumStr;
        do
        {
            NumStr += LastChar;
            LastChar = getchar();
        } while (isdigit(LastChar) || LastChar == '.');
        NumVal = strtod(NumStr.c_str(), 0);
        return tok_number;
    }

    if (LastChar == '#')
    {
        do
            LastChar = getchar();
        while (LastChar != EOF && LastChar != '\n' && LastChar != '\r');

        if (LastChar != EOF)
        {
            gettok();
        }
    }

    if (LastChar == EOF)
    {
        return tok_eof;
    }

    int ThisChar = LastChar;
    LastChar = getchar();
    return ThisChar;
}

class ExprAST
{
public:
    virtual ~ExprAST() {}
};

class NumberExprAst : public ExprAST
{
    double Val;

public:
    NumberExprAst(double V) : Val(V)
    {
    }
};

class VariableExprAST : public ExprAST
{
    std::string Name;

public:
    VariableExprAST(const std::string &Name) : Name(Name) {}
};

class BinaryExprAST : public ExprAST
{
    char Op;
    std::unique_ptr<ExprAST> LHS, RHS;

public:
    BinaryExprAST(
        char op,
        std::unique_ptr<ExprAST> LHS,
        std::unique_ptr<ExprAST> RHS) : Op(op), LHS(std::move(LHS)), RHS(std::move(RHS)) {}
};

class CallExprAST : public ExprAST
{
    std::string Callee;
    std::vector<std::unique_ptr<ExprAST>> Args; // dynamic array

public:
    CallExprAST(const std::string &Callee,
                std::vector<std::unique_ptr<ExprAST>> Args)
        : Callee(Callee), Args(std::move(Args)) {}
};

class PrototypeAST
{
    std::string Name;
    std::vector<std::string> Args;

public:
    PrototypeAST(const std::string &Name, std::vector<std::string> Args)
        : Name(Name), Args(std::move(Args)) {}

    const std::string &getName() const { return Name; }
};

/// FunctionAST - This class represents a function definition itself.
class FunctionAST
{
    std::unique_ptr<PrototypeAST> Proto;
    std::unique_ptr<ExprAST> Body;

public:
    FunctionAST(std::unique_ptr<PrototypeAST> Proto,
                std::unique_ptr<ExprAST> Body)
        : Proto(std::move(Proto)), Body(std::move(Body)) {}
};

static int CurTok;

static int getNextToken()
{
    return CurTok = gettok();
};

std::unique_ptr<ExprAST> LogError(const char *Str)
{
    fprintf(stderr, "LogError: %s\n", Str);
    return nullptr;
};

std::unique_ptr<PrototypeAST> LogErrorP(const char *Str)
{
    LogError(Str);
    return nullptr;
};

// Parse every expression
static std::unique_ptr<ExprAST> ParseNumberExpr()
{
    auto Result = std::make_unique<NumberExprAst>(NumVal);
    getNextToken(); // consume the number
    return std::move(Result);
}
static std::unique_ptr<ExprAST> ParseExpression();

static std::unique_ptr<ExprAST> ParseIdentifierOrCallExpr()
{
    std::string IdName = IdentifierStr;

    getNextToken(); // eatIdentifier
    if (CurTok != '(')
    {
        return std::make_unique<VariableExprAST>(IdName);
    }
    getNextToken(); // (
    std::vector<std::unique_ptr<ExprAST>> Args;

    if (CurTok != ')')
    {
        while (true)
        {
            if (auto Arg = ParseExpression())
            {
                Args.push_back(std::move(Arg));
            }
            else
            {
                return nullptr;
            }

            if (CurTok == ')')
            {
                break;
            }

            if (CurTok != ',')
            {
                return LogError("Expected ')' or ',' in argument List");
            }
            getNextToken();
        }
    }
    getNextToken();
    return std::make_unique<CallExprAST>(IdName, std::move(Args));
};
static std::unique_ptr<ExprAST> ParseParenExpr();

static std::unique_ptr<ExprAST> ParsePrimary()
{
    switch (CurTok)
    {

    default:
        return LogError("unknown token when expecting an expression");
    case tok_identifier:
        return ParseIdentifierOrCallExpr();
    case tok_number:
        return ParseNumberExpr();
    case '(':
        return ParseParenExpr();
    }
}
static std::unique_ptr<ExprAST> ParseBinOpRHS(int ExprPrec,
                                              std::unique_ptr<ExprAST> LHS);
/// expression
///   ::= primary binoprhs
///
static std::unique_ptr<ExprAST> ParseExpression()
{
    auto LHS = ParsePrimary();
    if (!LHS)
        return nullptr;

    return ParseBinOpRHS(0, std::move(LHS));
}
static int GetTokPrecedence();
static std::unique_ptr<ExprAST> ParseBinOpRHS(int ExprPrec,
                                              std::unique_ptr<ExprAST> LHS)
{
    while (true)
    {
        // get token
        int TokPrec = GetTokPrecedence();
        if (TokPrec < ExprPrec)
        {
            return LHS;
        }

        int BinOp = CurTok;
        getNextToken();
        auto RHS = ParsePrimary();
        if (!RHS)
        {
            return nullptr;
        }
        int NextPrec = GetTokPrecedence();
        if (TokPrec < NextPrec)
        {
            RHS = ParseBinOpRHS(TokPrec + 1, std::move(RHS));
            if (!RHS)
                return nullptr;
        }
        // Merge LHS/RHS.
        LHS = std::make_unique<BinaryExprAST>(BinOp, std::move(LHS),
                                              std::move(RHS));
    }
}

static std::unique_ptr<ExprAST> ParseParenExpr()
{
    getNextToken(); // eat '('
    auto V = ParseExpression();

    if (!V)
    {
        return nullptr;
    }

    if (CurTok != ')')
    {
        return LogError("expected ')'");
    }
    getNextToken();
    return V;
}

/// BinopPrecedence - This holds the precedence for each binary operator that is
/// defined.
static std::map<char, int> BinopPrecedence;

/// GetTokPrecedence - Get the precedence of the pending binary operator token.
static int GetTokPrecedence()
{
    if (!isascii(CurTok))
        return -1;
    switch (CurTok)
    {
    case '<':
    case '>':
        return 10;
    case '+':
    case '-':
        return 20;
    case '*':
    case '/':
        return 40;
    default:
        return 1;
    }
    // Make sure it's a declared binop.
    int TokPrec = BinopPrecedence[CurTok];
    if (TokPrec <= 0)
        return -1;
    return TokPrec;
};

/// prototype
///   ::= id '(' id* ')'
/// function signature
///
static std::unique_ptr<PrototypeAST> ParsePrototype()
{
    if (CurTok != tok_identifier)
        return LogErrorP("Expected function name in prototype");

    string FnName = IdentifierStr;
    getNextToken(); // eat the identifier

    if (CurTok != '(')
        return LogErrorP("Expected '(' in prototype");

    // Read the list of argument names.
    vector<string> ArgNames;
    while (getNextToken() == tok_identifier)
        ArgNames.push_back(IdentifierStr);
    if (CurTok != ')')
        return LogErrorP("Expected ')' in prototype");

    // success.
    getNextToken(); // eat ')'.

    return std::make_unique<PrototypeAST>(FnName, std::move(ArgNames));
}

/// defintion ::== def prototype
static std::unique_ptr<FunctionAST> ParseDefinition()
{
    getNextToken(); // eat the def
    auto Proto = ParsePrototype();
    if (!Proto)
    {
        return nullptr;
    }

    if (auto E = ParseExpression())
    {
        return std::make_unique<FunctionAST>(std::move(Proto), std::move(E));
    }
    return nullptr;
}

static std::unique_ptr<PrototypeAST> ParseExtern()
{
    getNextToken(); // eat extern
    return ParsePrototype();
}

/// toplevelexpr ::= expression
/// anonymous nullary functions
static std::unique_ptr<FunctionAST> ParseTopLevelExpr()
{
    if (auto E = ParseExpression())
    {
        // Make an anonymous proto.
        // no name and no args
        auto Proto = std::make_unique<PrototypeAST>("__anon_expr",
                                                    std::vector<std::string>());
        return std::make_unique<FunctionAST>(std::move(Proto), std::move(E));
    }
    return nullptr;
}

static void HandleDefinition()
{
    if (ParseDefinition())
    {
        fprintf(stderr, "Parsed a function definition.\n");
    }
    else
    {
        // Skip token for error recovery.
        getNextToken();
    }
}

static void HandleExtern()
{
    if (ParseExtern())
    {
        fprintf(stderr, "Parsed an extern\n");
    }
    else
    {
        // Skip token for error recovery.
        getNextToken();
    }
}

static void HandleTopLevelExpression()
{
    // Evaluate a top-level expression into an anonymous function.
    if (ParseTopLevelExpr())
    {
        fprintf(stderr, "Parsed a top-level expr\n");
    }
    else
    {
        // Skip token for error recovery.
        getNextToken();
    }
}

/// top ::= definition | external | expression | ';'
static void MainLoop()
{
    while (true)
    {
        fprintf(stderr, "ready> ");
        switch (CurTok)
        {
        case tok_eof:
            return;
        case ';': // ignore top-level semicolons.
            getNextToken();
            break;
        case tok_def:
            HandleDefinition();
            break;
        case tok_extern:
            HandleExtern();
            break;
        default:
            HandleTopLevelExpression();
            break;
        }
    }
}

int main()
{
    getNextToken();
    MainLoop();
    return 0;
}