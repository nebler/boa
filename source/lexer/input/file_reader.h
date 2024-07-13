#ifndef FILE_READER_H
#define FILE_READER_H

#include <fstream>
#include <iostream>
#include <string>

#include <studio.h>

#include "input_stream_reader.h"
class FileReader : public InputStreamReader
{
private:
  bool validateFile(const char* pathToFileName);
  FILE* file;

public:
  char readInput();
  FileReader(const char* pathToFile)
  {
    if (validateFile(pathToFile)) {
      this->file = fopen(pathToFile, "r");
    }
  }
};

#endif