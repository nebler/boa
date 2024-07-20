// token.h
#ifndef TOKEN_H
#define TOKEN_H

// The lexer returns tokens [0-255] if it is an unknown character, otherwise one
// of these for known things.
enum Token
{
  // var definition
  tok_var = -17,
  tok_eof = -1,

  // commands
  tok_def = -2,
  tok_extern = -3,

  // primary
  tok_identifier = -4,
  tok_number = -5,

  // control
  tok_if = -6,
  tok_then = -7,
  tok_else = -8,
  tok_for = -9,
  tok_in = -10,

  // operators
  tok_binary = -11,
  tok_unary = -12,
  tok_strcut = -13,
  tok_int = -14,
  tok_float = -15,
  tok_bool = -16
};

#endif  // TOKEN_H