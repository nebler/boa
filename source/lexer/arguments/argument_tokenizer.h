// lexer.h
#include <map>
#include <string>
#include <vector>

#include "argument_token.h"
using namespace std;
class ArgumentTokenizer
{
public:
public:
  ArgumentTokenizer(char* argv[])
      : argv(argv)
  {
  }
  vector<ArgTokens> GetArgumentTokens();

private:
  char** argv;
};