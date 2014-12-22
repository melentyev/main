/*
 * Copyright (c) 2013, 2014 Corvusoft
 */

//System Includes

//Project Includes
#include "corvusoft/framework/string.h"
#include "corvusoft/framework/string_option.h"
#include "corvusoft/framework/detail/string_impl.h"

//External Includes

//System Namespaces
using std::map;
using std::string;
using std::vector;
using std::multimap;

//Project Namespaces
using framework::detail::StringImpl;

//External Namespaces

namespace framework
{
    const char* String::empty = StringImpl::empty;
    
    Bytes String::to_bytes( const string& value )
    {
        return StringImpl::to_bytes( value );
    }
    
    string String::to_string( const Bytes& value )
    {
        return StringImpl::to_string( value );
    }
    
    string String::lowercase( const string& value )
    {
        return StringImpl::lowercase( value );
    }
    
    string String::uppercase( const string& value )
    {
        return StringImpl::uppercase( value );
    }
    
    string String::format( const string format, ... )
    {
        va_list arguments;
        va_start( arguments, format );
        
        string formatted = StringImpl::format( format, arguments );
        
        va_end( arguments );
        
        return formatted;
    }
    
    vector< string > String::split( const string& value, const char delimiter )
    {
        return StringImpl::split( value, delimiter );
    }
    
    string String::join( const vector< string >& values, const string& delimiter )
    {
        return StringImpl::join( values, delimiter );
    }
    
    string String::join( const map< string, string >& values, const string& pair_delimiter, const string& delimiter )
    {
        return StringImpl::join( values, pair_delimiter, delimiter );
    }
    
    string String::join( const multimap< string, string >& values, const string& pair_delimiter, const string& delimiter )
    {
        return StringImpl::join( values, pair_delimiter, delimiter );
    }
    
    string String::trim( const string& value, const string& delimiter )
    {
        return StringImpl::trim( value, delimiter );
    }
    
    string String::trim_leading( const string& value, const string& delimiter )
    {
        return StringImpl::trim_leading( value, delimiter );
    }
    
    string String::trim_lagging( const string& value, const string& delimiter )
    {
        return StringImpl::trim_lagging( value, delimiter );
    }
    
    string String::remove( const string& target, const string& value )
    {
        return StringImpl::remove( target, value, StringOption::CASE_SENSITIVE );
    }
    
    string String::remove( const string& target, const string& value, const StringOption option )
    {
        return StringImpl::remove( target, value, option );
    }
    
    string String::replace( const string& target, const string& substitute, const string& value )
    {
        return StringImpl::replace( target, substitute, value, StringOption::CASE_SENSITIVE );
    }
    
    string String::replace( const string& target, const string& substitute, const string& value, const StringOption option )
    {
        return StringImpl::replace( target, substitute, value, option );
    }
}
