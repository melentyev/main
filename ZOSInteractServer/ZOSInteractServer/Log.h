#include <iostream>
#include <string>

class Log
{
public:
    static void console(const std::string& s)
    {
        std::cout << s << std::endl;
    }
};