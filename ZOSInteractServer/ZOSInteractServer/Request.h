#ifndef _REQUEST_H
#define _REQUEST_H 1

#include <map>
#include <stdint.h>
#include <string>
#include <vector>

#include "Method.h"

class Request
{
public:
    Request();
    Method get_method(void) const;
    void set_method(const Method &m);
    std::string get_header(const std::string &key) const;
    void set_header(const std::string &name, const std::string &value);
    std::string get_path() const;
    void set_path(const std::string &path);
    std::string get_query_param(const std::string &param_name) const;
    std::map<std::string, std::string> &query_params();
    std::string& protocol();
    std::string& version();
    int content_length;
    std::vector<uint8_t> body;
private:
    std::map<std::string, std::string> m_headers;
    Method m_method;
    std::string m_path, m_version, m_protocol;
    std::map<std::string, std::string> m_query_params;
};

#endif