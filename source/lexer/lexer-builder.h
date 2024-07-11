// lexer-builder.h
#ifndef LEXER_BUILDER_H
#define LEXER_BUILDER_H
#include <iostream>
#include <memory>
#include <vector>

#include "arguments/argument_token.h"
#include "arguments/argument_tokenizer.h"
#include "input/cli_reader.h"
#include "input/file_reader.h"
#include "tokenizer/tokenizer.cpp"

Tokenizer static createTokenizer(char* argv[])
{
  vector<ArgTokens> tokens = ArgumentTokenizer(argv).GetArgumentTokens();
  for (const ArgTokens& token : tokens) {
    if (token == cli_mode) {
      std::cout << "CLI Mode" << std::endl;
      return Tokenizer(std::make_unique<CliReader>());
    }
  }
  const char* str = argv[1];
  return Tokenizer(std::make_unique<FileReader>(str));
}

#endif