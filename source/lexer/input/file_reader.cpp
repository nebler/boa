#include "file_reader.h"
char FileReader::readInput()
{

    return getc(this->file);
}

bool FileReader::validateFile(const char *pathToFileName)
{
    return true;
}