/*
 * Copyright (c) 2013, 2014 Corvusoft
 */

#ifndef _FRAMEWORK_PASSWORD_H
#define _FRAMEWORK_PASSWORD_H 1

//System Includes
#include <memory>
#include <string>

//Project Includes
#include <corvusoft/framework/resident_string>

//External Includes

//System Namespaces

//Project Namespaces

//External Namespaces

namespace framework
{
    //Forward Declarations
    enum HashAlgorithm :
    int;
    
    namespace detail
    {
        class PasswordImpl;
    }
    
    class Password
    {
        public:
            //Friends
            
            //Definitions
            
            //Constructors
            Password( const std::string& hash );
            
            Password( const Password& original );
            
            Password( const detail::PasswordImpl& implementation );
            
            virtual ~Password( void );
            
            //Functionality
            std::string to_string( void ) const;
            
            static Password parse( const std::string& hash );
            
            static Password generate( const ResidentString& cleartext );
            
            static Password generate( const ResidentString& cleartext, const std::string& salt );
            
            static Password generate( const ResidentString& cleartext, const std::string& salt, const HashAlgorithm algorithm );
            
            //Getters
            
            //Setters
            
            //Operators
            Password& operator =( const Password& value );
            
            bool operator ==( const Password& value ) const;
            
            bool operator !=( const Password& value ) const;
            
            bool operator ==( const ResidentString& cleartext ) const;
            
            bool operator !=( const ResidentString& cleartext ) const;
            
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
            Password( void );
            
            //Functionality
            
            //Getters
            
            //Setters
            
            //Operators
            bool operator <( const Password& value ) const = delete;
            
            bool operator >( const Password& value ) const = delete;
            
            //Properties
            const std::unique_ptr< detail::PasswordImpl > m_pimpl;
    };
}

#endif  /* _FRAMEWORK_PASSWORD_H */
