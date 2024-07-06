// lexer-builder.h
#ifndef LEXER_BUILDER_H
#define LEXER_BUILDER_H
#include <vector>
#include "tokenizer/tokenizer.cpp"
#include "arguments/argument_token.h"
#include "arguments/argument_tokenizer.h"
#include "input/file_reader.h"
#include "input/cli_reader.h"
#include <memory>
#include <iostream>

Tokenizer static createTokenizer(char *argv[])
{
    vector<ArgTokens> tokens = ArgumentTokenizer(argv).GetArgumentTokens();
    for (const ArgTokens &token : tokens)
    {
        if (token == cli_mode)
        {
            std::cout << "if clause" << std::endl;
            return Tokenizer(std::make_unique<CliReader>());
        }
    }
    const char *str = argv[1];
    std::cout << "string is" << std::endl;
    std::cout << str << std::endl;
    return Tokenizer(std::make_unique<FileReader>(str));
}

#endif