// lexer.h
#include <string>
#include "token.h"

// Declare the global variables used by gettok
static std::string IdentifierStr; // Filled in if tok_identifier
static double NumVal;             // Filled in if tok_number
static int CurTok;

// Declare the gettok function

static int GetTokPrecedence(const std::map<char, int> &BinopPrecedence);
static int getNextToken();