#include "rest_framework.h"
#include "StringUtil.h"
#include <sstream>

Response::Response(void) : m_body(), status_code(0)
{
    headers.clear();
    m_version = "1.1";
    status_message = "OK";
}

void Response::set_body(char buf[], int size)
{
    copy(buf, buf + size, back_inserter(m_body));
}

void Response::set_body(const string& value)
{
    m_body = value;
}

std::string Response::to_string()
{
    std::stringstream builder;
    builder << "HTTP/" << this->m_version << " " << status_code << " " 
        << status_message << StringUtil::http_eol;
    
    headers[StringUtil::content_length] = StringUtil::to_string(m_body.length());
    for (std::map<std::string, std::string>::const_iterator it = headers.begin(); 
        it != headers.end(); it++)
    {
        builder << it->first << ": " << it->second << StringUtil::http_eol;
    }
    builder << StringUtil::http_eol;
    builder << m_body;
    std::string  result = builder.str();
    return result;
}