#include <iostream>

extern "C"
{
    double average(double, double);
}

extern "C"
{
    double printd(double);
}

int main()
{
    std::cout << "average of 3.0 and 4.0: " << average(3.0, 4.0) << std::endl;
}