#include "input_stream_reader.h"
#include <string>
#include <fstream>
class CliReader : public InputStreamReader
{
private:
public:
    char readInput();
    CliReader(){};
};