#include <string>
#include <iostream>
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

int main()
{
    while (true)
    {
        int tok = gettok();
        cout << "got token: " << tok << endl;
    }
}