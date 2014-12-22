/*
 * Copyright (c) 2013, 2014 Corvusoft
 */

#ifndef _FRAMEWORK_DETAIL_USERNAME_IMPL_H
#define _FRAMEWORK_DETAIL_USERNAME_IMPL_H 1

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
        //Forward Declarations
        
        class UsernameImpl
        {
            public:
                //Friends
                
                //Definitions
                
                //Constructors
                UsernameImpl( void );
                
                UsernameImpl( const UsernameImpl& original );
                
                virtual ~UsernameImpl( void );
                
                //Functionality
                std::string to_string( void ) const;
                
                static bool is_valid( const std::string& value );
                
                //Getters
                
                //Setters
                void set_username( const std::string& value );
                
                //Operators
                bool operator <( const UsernameImpl& value ) const;
                
                bool operator >( const UsernameImpl& value ) const;
                
                bool operator ==( const UsernameImpl& value ) const;
                
                bool operator !=( const UsernameImpl& value ) const;
                
                UsernameImpl& operator =( const UsernameImpl& value );
                
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
                std::string m_username;
        };
    }
}

#endif  /* _FRAMEWORK_DETAIL_USERNAME_IMPL_H */
