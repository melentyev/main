#include "rest_framework.h"
#include "StringUtil.h"
#include "StatusCode.h"

#define FOR_EACH(type, itname, coll) for (type::iterator itname = (coll).begin(); itname != (coll).end(); itname++)

Request::Request()
{

}

std::map<std::string, std::string> &Request::query_params()
{
    return m_query_params;
}

std::string &Request::version()
{
    return m_version;
}

std::string &Request::protocol()
{
    return m_protocol;
}

std::string Request::get_query_param(const string& param_name) const
{
    return m_query_params.find(param_name)->second;
}

void Request::set_method(const Method &m)
{
    m_method = m;
}

Method Request::get_method() const
{
    return m_method;
}

string Request::get_header(const string &key) const
{
    return (m_headers.find(key))->second;
}
void Request::set_header(const string &name, const string &value)
{
    m_headers[name] = value;
}

string Request::get_path() const
{
    return m_path;
}
void Request::set_path(const string &path)
{
    m_path = path;
}