#ifndef TOKENIZER_H
#define TOKENIZER_H
// lexer.h
#include <map>
#include <memory>
#include <string>

#include "../input/input_stream_reader.h"
#include "token.h"

class Tokenizer
{
private:
  std::unique_ptr<InputStreamReader> reader;
  int gettok();

public:
  Tokenizer(std::unique_ptr<InputStreamReader> reader)
      : reader(std::move(reader))
  {
  }

  int getNextToken();
  int GetTokPrecedence(const std::map<char, int>& BinopPrecedence);
};

#endif  // TOKENIZER_H