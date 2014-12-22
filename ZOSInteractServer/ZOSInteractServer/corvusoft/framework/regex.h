/*
 * Copyright (c) 2013, 2014 Corvusoft
 */

#ifndef _FRAMEWORK_REGEX_H
#define _FRAMEWORK_REGEX_H 1

//System Includes
#include <string>

//Project Includes

//External Includes

//System Namespaces

//Project Namespaces

//External Namespaces

namespace framework
{
    //Forward Declarations
    namespace detail
    {
        class RegexImpl;
    }
    
    class Regex
    {
        public:
            //Friends
            
            //Definitions
            
            //Constructors
            Regex( void );
            
            Regex( const char* pattern, const int options = 0 );
            
            Regex( const std::string& pattern, const int options = 0 );
            
            Regex( const Regex& original );
            
            Regex( const detail::RegexImpl& implementation );
            
            virtual ~Regex( void );
            
            //Functionality
            std::string to_string( void ) const;
            
            bool is_match( const std::string& value );
            
            static bool is_match( const std::string& value, const std::string& pattern, const int options = 0 );
            
            static bool is_valid( const std::string& value );
            
            //Getters
            
            //Setters
            
            //Operators
            Regex& operator =( const Regex& value );
            
            bool operator <( const Regex& value ) const;
            
            bool operator >( const Regex& value ) const;
            
            bool operator ==( const Regex& value ) const;
            
            bool operator !=( const Regex& value ) const;
            
            //Properties
            
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
            
            //Functionality
            
            //Getters
            
            //Setters
            
            //Operators
            
            //Properties
            std::unique_ptr< detail::RegexImpl > m_pimpl;
    };
}

#endif  /* _FRAMEWORK_REGEX_H */
