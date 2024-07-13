#include <fstream>
#include <string>

#include "input_stream_reader.h"
class CliReader : public InputStreamReader
{
private:
public:
  char readInput();
  CliReader() {};
};