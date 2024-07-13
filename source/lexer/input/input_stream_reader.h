#ifndef INPUT_STREAM_READER_H
#define INPUT_STREAM_READER_H

class InputStreamReader
{
public:
  virtual char readInput() = 0;
  virtual ~InputStreamReader() = default;
};

#endif