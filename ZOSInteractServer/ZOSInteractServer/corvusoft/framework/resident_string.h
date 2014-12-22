/*
 * Copyright (c) 2013, 2014 Corvusoft
 */

#ifndef _FRAMEWORK_RESIDENT_STRING_H
#define _FRAMEWORK_RESIDENT_STRING_H 1

//System Includes
#include <string>

//Project Includes
#include <corvusoft/framework/resident_allocator>

//External Includes

//System Namespaces

//Project Namespaces

//External Namespaces

//Forward Declarations

namespace framework
{
    typedef std::basic_string< char, std::char_traits< char >, ResidentAllocator< char > > ResidentString;
}

#endif  /* _FRAMEWORK_RESIDENT_STRING_H */
