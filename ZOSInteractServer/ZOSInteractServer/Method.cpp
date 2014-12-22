#include "Method.h"
#include "StatusCode.h"
#include "StringUtil.h"
#include <algorithm>

Method::Method()
{

}

Method::Method(const char* value)
{
    setup(value);
}

Method::Method(const std::string& value)
{
    setup(value);
}

void Method::setup(const std::string& value)
{
    int cnt = 8;
    std::string methods[8] = {
        "GET", "PUT",
        "POST", "HEAD",
        "TRACE", "DELETE",
        "CONNECT", "OPTIONS"
    };
    m_method = StringUtil::uppercase(value);
    if (std::find(methods, methods + 8, m_method) == methods + cnt)
    {
        throw StatusCode::METHOD_NOT_ALLOWED;
    }
}

bool Method::operator==(const Method &m) const
{
    return this->m_method == m.m_method;
}