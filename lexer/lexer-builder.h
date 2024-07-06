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

Tokenizer static createTokenizer(char *argv[])
{
    vector<ArgTokens> foo = ArgumentTokenizer(argv).GetArgumentTokens();
    if (std::find(foo.begin(), foo.end(), cli_mode) != foo.end())
    {
        return Tokenizer(std::make_unique<CliReader>());
    }
    const char *str = argv[1];
    return Tokenizer(std::make_unique<FileReader>(str));
}

#endif