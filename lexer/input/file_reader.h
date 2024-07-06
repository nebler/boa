#ifndef FILE_READER_H
#define FILE_READER_H

#include "input_stream_reader.h"
#include <string>
#include <fstream>
#include <stdio.h>
class FileReader : public InputStreamReader
{
private:
    bool validateFile(const char *pathToFileName);
    FILE *file;

public:
    char readInput();
    FileReader(const char *pathToFile)
    {
        if (validateFile(pathToFile))
        {
            this->file = fopen(pathToFile, "r");
        }
    }
};

#endif