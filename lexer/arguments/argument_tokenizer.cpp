#include <string>
#include <map>
#include <vector>
#include <iostream>
#include "argument_token.h"

using namespace std;
class ArgumentTokenizer
{

public:
    ArgumentTokenizer(char *argv[]) : argv(argv)
    {
    }
    vector<ArgTokens> GetArgumentTokens();

private:
    char **argv;
};

vector<ArgTokens> ArgumentTokenizer::GetArgumentTokens()
{
    std::vector<ArgTokens> tokens;
    for (int i = 0; argv[i] != nullptr; ++i)
    {
        std::string arg_str = argv[i];
        std::cout << arg_str << std::endl;
        try
        {
            ArgTokens token = FromString(arg_str);
            tokens.push_back(token);
        }
        catch (const std::invalid_argument &e)
        {
            std::cerr << "Warning: Unrecognized argument \"" << arg_str << "\"." << std::endl;
        }
    }
    std::cout << "return tokens" << std::endl;
    return tokens;
}