/*
 * Copyright (c) 2013, 2014 Corvusoft
 */

#ifndef _FRAMEWORK_STRING_H
#define _FRAMEWORK_STRING_H 1

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
    
    class String
    {
        public:
            //Friends
            
            //Definitions
            
            //Constructors
            
            //Functionality
            static Bytes to_bytes( const std::string& value );
            
            static std::string to_string( const Bytes& value );
            
            static std::string lowercase( const std::string& value );
            
            static std::string uppercase( const std::string& value );
            
            static std::string format( const std::string format, ... );
            
            static std::vector< std::string > split( const std::string& text, const char delimiter );
            
            static std::string join( const std::vector< std::string >& values, const std::string& delimiter );
            
            static std::string join( const std::map< std::string, std::string >& values, const std::string& pair_delimiter, const std::string& delimiter );
            
            static std::string join( const std::multimap< std::string, std::string >& values, const std::string& pair_delimiter, const std::string& delimiter );
            
            static std::string trim( const std::string& value, const std::string& delimiter = " \t\r\n" );
            
            static std::string trim_leading( const std::string& value, const std::string& delimiter = " \t\r\n" );
            
            static std::string trim_lagging( const std::string& value, const std::string& delimiter = " \t\r\n" );
            
            static std::string remove( const std::string& needle, const std::string& haystack );
            
            static std::string remove( const std::string& needle, const std::string& haystack, const StringOption option );
            
            static std::string replace( const std::string& target, const std::string& substitute, const std::string& value );
            
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
            String( void ) = delete;
            
            String( const String& original ) = delete;
            
            virtual ~String( void ) = delete;
            
            //Functionality
            
            //Getters
            
            //Setters
            
            //Operators
            String& operator =( const String& value ) = delete;
            
            //Properties
    };
}

#endif  /* _FRAMEWORK_STRING_H */
