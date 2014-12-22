/*
 * Copyright (c) 2013, 2014 Corvusoft
 */

#ifndef _FRAMEWORK_UNIQUE_ID_H
#define _FRAMEWORK_UNIQUE_ID_H 1

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
    //Forward Delcarations
    namespace detail
    {
        class UniqueIdImpl;
    }
    
    class UniqueId
    {
        public:
            //Friends
            
            //Definitions
            
            //Constructors
            UniqueId( const std::string& value );
            
            UniqueId( const UniqueId& original );
            
            UniqueId( const detail::UniqueIdImpl& implementation );
            
            virtual ~UniqueId( void );
            
            //Functionality
            std::string to_string( void ) const;
            
            static UniqueId generate( void );
            
            static UniqueId parse( const std::string& value );
            
            static bool is_valid( const std::string& value );
            
            //Getters
            
            //Setters
            
            //Operators
            UniqueId& operator =( const UniqueId& value );
            
            bool operator <( const UniqueId& value ) const;
            
            bool operator >( const UniqueId& value ) const;
            
            bool operator ==( const UniqueId& value ) const;
            
            bool operator !=( const UniqueId& value ) const;
            
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
            UniqueId( void );
            
            //Functionality
            
            //Getters
            
            //Setters
            
            //Operators
            
            //Properties
            const std::unique_ptr< detail::UniqueIdImpl > m_pimpl;
    };
}

#endif  /* _FRAMEWORK_UNIQUE_ID_H */
