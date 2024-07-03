#include <fstream>
#include <stdexcept>

class FileReader : public InputReader
{
public:
    FileReader(const std::string &path) : file(path), bufferIndex(0), bufferSize(0)
    {
        if (!file.is_open())
        {
            throw std::runtime_error("Unable to open file");
        }
    }

    char getInput() override
    {
        if (bufferIndex >= bufferSize)
        {
            fillBuffer();
            if (bufferSize == 0)
            { // End of file
                return EOF;
            }
        }
        return buffer[bufferIndex++];
    }

private:
    void fillBuffer()
    {
        if (file.eof())
        {
            bufferSize = 0;
            return;
        }
        file.read(buffer, sizeof(buffer));
        bufferSize = file.gcount();
        bufferIndex = 0;
    }

    std::ifstream file;
    static const int BUFFER_SIZE = 1024;
    char buffer[BUFFER_SIZE];
    std::size_t bufferIndex;
    std::size_t bufferSize;
};
