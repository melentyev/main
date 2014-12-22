#include <stdint.h>
#include <algorithm>
#include <string>
#include <sstream>

#include "StringUtil.h"

const std::string StringUtil::http_eol = "\r\n";
const std::string StringUtil::whitespace = " ";
const std::string StringUtil::content_length = "Content-Length";

#ifdef WIN32
std::string StringUtil::ascii_to_ebcdic(const std::string &value)
{
    return value;
}

std::string StringUtil::ebcdic_to_ascii(const std::string &value)
{
    return value;
}
#else
std::string StringUtil::ascii_to_ebcdic(const std::string &value)
{
    return value;
}

std::string StringUtil::ebcdic_to_ascii(const std::string &value)
{
    return value;
}
#endif

template<class T>
std::string _to_string(T value)
{
    std::stringstream ss;
    ss << value;
    return ss.str();
}

std::string StringUtil::to_string(int value)
{
    return _to_string(value);
}

std::string StringUtil::to_string(unsigned int value)
{
    return _to_string(value);
}

std::string StringUtil::uppercase(const std::string &value)
{
    std::string result = "";
    std::transform(value.begin(), value.end(), back_inserter(result), (int(*)(int))toupper);
    return result;
}

std::string StringUtil::lowercase(const std::string& value)
{
    std::string result = "";
    std::transform(value.begin(), value.end(), back_inserter(result), (int(*)(int))tolower);
    return result;
}

std::string StringUtil::trim_whitespace(const std::string& value)
{
    int l = 0, r = value.length() - 1;
    while (value[l] == ' ' && l < (int)value.length()) l++;
    while (value[r] == ' ' && r >= 0) r--;
    return l > r ? "" : value.substr(l, r - l + 1);
}

std::vector<std::string>& StringUtil::split(const std::string &s, 
    const std::string &delim, std::vector<std::string> &elems, 
    bool remove_empty, uint32_t max_parts)
{
    std::string item;
    int pos = 0, new_pos = 0;
    while ((new_pos = s.find(delim, pos)) != std::string::npos
        && (max_parts == 0 || (int)elems.size() < max_parts))
    {
        item = s.substr(pos, new_pos - pos);
        if (!remove_empty || item != "")
        {
            elems.push_back(item);
        }
        pos = new_pos + delim.length();
    }
    item = s.substr(pos, s.length() - pos);
    if ((max_parts == 0 || elems.size() < max_parts) && item.length() != 0)
    {
        elems.push_back(item);
    }
    return elems;
}

std::vector<std::string> StringUtil::split(const std::string &s, 
    const std::string &delim, bool remove_empty, int max_parts)
{
    std::vector<std::string> elems;
    StringUtil::split(s, delim, elems, remove_empty, max_parts);
    return elems;
}