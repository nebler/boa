// token.h
#ifndef ARGUMENT_TOKEN_H
#define ARGUMENT_TOKEN_H
enum ArgTokens
{
    cli_mode = 1
};

// Function to get the string representation of an enum value
// todo: check if c++ offers a better way
inline std::string ToString(ArgTokens token)
{
    switch (token)
    {
    case cli_mode:
        return "-i";
    default:
        return "";
    }
}

// Function to get the enum value from a string representation
inline ArgTokens FromString(const std::string &str)
{
    static const std::unordered_map<std::string, ArgTokens> token_map = {
        {"-i", cli_mode},
        // Add more mappings as needed
    };

    auto it = token_map.find(str);
    if (it != token_map.end())
    {
        return it->second;
    }
    throw std::invalid_argument("Invalid argument token string");
}

#endif // TOKEN_H