#include "tokenizer.h"

// Define the global variables used by gettok
int CurTok;
std::string IdentifierStr; // Filled in if tok_identifier
double NumVal;             // Filled in if tok_number

int Tokenizer::gettok()
{
    static int LastChar = ' ';

    // Skip any whitespace.
    while (isspace(LastChar))
        LastChar = this->reader->readInput();

    if (isalpha(LastChar))
    { // identifier: [a-zA-Z][a-zA-Z0-9]*
        IdentifierStr = LastChar;
        while (isalnum((LastChar = this->reader->readInput())))
            IdentifierStr += LastChar;

        if (IdentifierStr == "def")
            return tok_def;
        if (IdentifierStr == "extern")
            return tok_extern;
        if (IdentifierStr == "if")
            return tok_if;
        if (IdentifierStr == "then")
            return tok_then;
        if (IdentifierStr == "else")
            return tok_else;
        if (IdentifierStr == "for")
            return tok_for;
        if (IdentifierStr == "in")
            return tok_in;
        if (IdentifierStr == "binary")
            return tok_binary;
        if (IdentifierStr == "unary")
            return tok_unary;
        if (IdentifierStr == "var")
            return tok_var;
        return tok_identifier;
    }

    if (isdigit(LastChar) || LastChar == '.')
    { // Number: [0-9.]+
        std::string NumStr;
        do
        {
            NumStr += LastChar;
            LastChar = this->reader->readInput();
        } while (isdigit(LastChar) || LastChar == '.');

        NumVal = strtod(NumStr.c_str(), nullptr);
        return tok_number;
    }

    if (LastChar == '#')
    {
        // Comment until end of line.
        do
            LastChar = this->reader->readInput();
        while (LastChar != EOF && LastChar != '\n' && LastChar != '\r');

        if (LastChar != EOF)
            return gettok();
    }

    // Check for end of file.  Don't eat the EOF.
    if (LastChar == EOF)
        return tok_eof;

    // Otherwise, just return the character as its ASCII value.
    int ThisChar = LastChar;
    LastChar = this->reader->readInput();
    return ThisChar;
}

int Tokenizer::getNextToken()
{
    return CurTok = gettok();
}

int Tokenizer::GetTokPrecedence(const std::map<char, int> &BinopPrecedence)
{
    if (!isascii(CurTok))
        return -1;

    // Make sure it's a declared binop.
    auto it = BinopPrecedence.find(CurTok);
    if (it == BinopPrecedence.end())
        return -1;
    return it->second;
}
