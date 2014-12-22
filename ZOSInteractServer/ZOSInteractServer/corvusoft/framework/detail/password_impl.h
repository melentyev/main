/*
 * Copyright (c) 2013, 2014 Corvusoft
 */

#ifndef _FRAMEWORK_DETAIL_PASSWORD_IMPL_H
#define _FRAMEWORK_DETAIL_PASSWORD_IMPL_H 1

//System Includes
#include <string>

//Project Includes
#include "corvusoft/framework/resident_string.h"

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
        //Forward Declarations
        
        class PasswordImpl
        {
            public:
                //Friends
                
                //Definitions
                
                //Constructors
                PasswordImpl( void );
                
                PasswordImpl( const PasswordImpl& original );
                
                virtual ~PasswordImpl( void );
                
                //Functionality
                std::string to_string( void ) const;
                
                static PasswordImpl generate( const ResidentString& cleartext );
                
                static PasswordImpl generate( const ResidentString& cleartext, const std::string& salt );
                
                static PasswordImpl generate( const ResidentString& cleartext, const std::string& salt, const HashAlgorithm algorithm );
                
                //Getters
                
                //Setters
                void set_password( const std::string& value );
                
                //Operators
                PasswordImpl& operator =( const PasswordImpl& value );
                
                bool operator ==( const PasswordImpl& value ) const;
                
                bool operator !=( const PasswordImpl& value ) const;
                
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
                
                //Functionality
                static std::string generate_salt( std::string::size_type size = 16 );
                
                //Getters
                
                //Setters
                
                //Operators
                bool operator <( const PasswordImpl& value ) const = delete;
                
                bool operator >( const PasswordImpl& value ) const = delete;
                
                //Properties
                std::string m_hash;
        };
    }
}

#endif  /* _FRAMEWORK_DETAIL_PASSWORD_IMPL_H */
