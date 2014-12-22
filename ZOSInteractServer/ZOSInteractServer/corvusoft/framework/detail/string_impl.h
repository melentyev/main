/*
 * Copyright (c) 2013, 2014 Corvusoft
 */

#ifndef _FRAMEWORK_DETAIL_STRING_IMPL_H
#define _FRAMEWORK_DETAIL_STRING_IMPL_H 1

//System Includes
#include <map>
#include <string>
#include <vector>
#include <cstdarg>

//Project Includes
#include <corvusoft/framework/bytes>

//External Includes

//System Namespaces

//Project Namespaces

//External Namespaces

namespace framework
{
    //Forward Declarations
    enum StringOption : int;
    
    namespace detail
    {
        //Forward Declarations
        
        class StringImpl
        {
            public:
                //Friends
                
                //Definitions
                
                //Constructor
                
                //Functionality
                static Bytes to_bytes( const std::string& value );
                
                static std::string to_string( const Bytes& value );
                
                static std::string lowercase( const std::string& value );
                
                static std::string uppercase( const std::string& value );
                
                static std::string format( const std::string format, va_list arguments );
                
                static std::vector< std::string > split( const std::string& text, const char delimiter );
                
                static std::string join( const std::vector< std::string >& values, const std::string& delimiter );
                
                static std::string join( const std::map< std::string, std::string >& values, const std::string& pair_delimiter, const std::string& delimiter );
                
                static std::string join( const std::multimap< std::string, std::string >& values, const std::string& pair_delimiter, const std::string& delimiter );
                
                static std::string trim( const std::string& value, const std::string& delimiter );
                
                static std::string trim_leading( const std::string& value, const std::string& delimiter );
                
                static std::string trim_lagging( const std::string& value, const std::string& delimiter );
                
                static std::string remove( const std::string& target, const std::string& value, const StringOption option );
                
                static std::string replace( const std::string& target, const std::string& substitute, const std::string& value, const StringOption option );
                
                //Getters
                
                //Setters
                
                //Operators
                
                //Properties
                static const char* empty;
                
            protected:
                //Friends
                
                //Definitions
                
                //Constructors
                
                //Functionality
                
                //Getters
                
                //Setters
                
                //Operators
                
                //Properties
                
            private:
                //Friends
                
                //Definitions
                
                //Constructors
                StringImpl( void ) = delete;
                
                StringImpl( const StringImpl& original ) = delete;
                
                virtual ~StringImpl( void ) = delete;
                
                //Functionality
                static std::string::size_type format( /*out*/ std::string& output, const std::string::size_type length, const std::string format, va_list arguments );
                
                //Getters
                
                //Setters
                
                //Operators
                StringImpl& operator =( const StringImpl& value ) = delete;
                
                //Properties
        };
    }
}

#endif  /* _FRAMEWORK_DETAIL_STRING_IMPL_H */
