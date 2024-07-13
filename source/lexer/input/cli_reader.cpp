#include "cli_reader.h"

// todo: combine the two classes
char CliReader::readInput()
{
  return getc(stdin);
}