/*
 * Copyright (c) 2013, 2014 Corvusoft
 */

#ifndef _FRAMEWORK_USERNAME_H
#define _FRAMEWORK_USERNAME_H 1

//System Includes
#include <memory>
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
        class UsernameImpl;
    }
    
    class Username
    {
        public:
            //Friends
            
            //Definitions
            
            //Constructors
            Username( const std::string& value );
            
            Username( const Username& original );
            
            Username( const detail::UsernameImpl& implementation );
            
            virtual ~Username( void );
            
            //Functionality
            std::string to_string( void ) const;
            
            static bool is_valid( const std::string& value );
            
            static Username parse( const std::string& value );
            
            //Getters
            
            //Setters
            
            //Operators
            Username& operator =( const Username& value );
            
            bool operator <( const Username& value ) const;
            
            bool operator >( const Username& value ) const;
            
            bool operator ==( const Username& value ) const;
            
            bool operator !=( const Username& value ) const;
            
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
            Username( void );
            
            //Functionality
            
            //Getters
            
            //Setters
            
            //Operators
            
            //Properties
            const std::unique_ptr< detail::UsernameImpl > m_pimpl;
    };
}

#endif  /* _FRAMEWORK_USERNAME_H */
