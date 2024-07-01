#include <string>
#include <map>
#include <vector>
#include <iostream>
#include "argument_token.h"

using namespace std;
class ArgumentTokenizer
{

public:
    ArgumentTokenizer(char *argv[]) : argv(argv) {}
    vector<ArgTokens> GetArgumentTokens();

private:
    char **argv;
};

vector<ArgTokens> ArgumentTokenizer::GetArgumentTokens()
{
    vector<ArgTokens> tokens;
    for (int i = 0; argv[i] != nullptr; ++i)
    {
        tokens.emplace_back(argv[i]);
    }
    std::cout << "we have a token" << std::endl;
    tokens.push_back(cli_mode);
    return tokens;
}