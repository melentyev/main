/*
 * Copyright (c) 2013, 2014 Corvusoft
 */

#ifndef _FRAMEWORK_HASH_ALGORITHM_H
#define _FRAMEWORK_HASH_ALGORITHM_H 1

//System Includes

//Project Includes

//External Includes

//System Namespaces

//Project Namespaces

//External Namespaces

//Forward Declarations

namespace framework
{
    enum HashAlgorithm :
    int
    {
        NONE          =  0,
        MD4           =  1,
        MD5           =  2,
        HAVAL         =  3,
        RMD160        =  4,
        TIGER         =  5,
        TIGER1        =  6,
        TIGER2        =  7,
        SHA1          =  8,
        SHA224        =  9,
        SHA256        = 10,
        SHA384        = 11,
        SHA512        = 12,
        WHIRLPOOL     = 13,
        CRC32         = 14,
        CRC32_RFC1510 = 15,
        CRC24_RFC2440 = 16,
    };
}

#endif  /* _FRAMEWORK_HASH_ALGORITHM_H */
