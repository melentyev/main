#ifndef _MY_STRING_UTIL_H
#define _MY_STRING_UTIL_H 1

#include <iostream>
#include <cstdio>
#include <cstring>
#include <string>
#include <vector>
#include <stdint.h>

class StringUtil
{
public:
    static std::string ascii_to_ebcdic(const std::string &value);

    static std::string ebcdic_to_ascii(const std::string &value);

    static std::string to_string(int value);

    static std::string to_string(unsigned int value);

    static std::string uppercase(const std::string &value);

    static std::string lowercase(const std::string& value);

    static std::string trim_whitespace(const std::string& value);

    static std::vector<std::string> &split(const std::string &s, const std::string &delim,
        std::vector<std::string> &elems, bool remove_empty = true,
        uint32_t max_parts = 0);

    static std::vector<std::string> split(const std::string &s, const std::string &delim,
        bool remove_empty = true, int max_parts = 0);
    static const std::string http_eol;
    static const std::string whitespace;
    static const std::string content_length;
};

#endif