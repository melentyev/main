#ifndef _METHOD_H
#define _METHOD_H 1

#include <string>

class Method
{
private:
    std::string m_method;
public:
    Method();
    Method(const char* value);
    Method(const std::string& value);
    void setup(const std::string& value);
    bool operator==(const Method &m) const;
};

#endif