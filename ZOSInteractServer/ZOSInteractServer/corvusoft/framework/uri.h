/*
 * Copyright (c) 2013, 2014 Corvusoft
 */

#ifndef _FRAMEWORK_URI_H
#define _FRAMEWORK_URI_H 1

//System Includes
#include <memory>
#include <string>

//Project Includes
#include <corvusoft/framework/bytes>

//External Includes

//System Namespaces

//Project Namespaces

//External Namespaces

namespace framework
{
    //Forward Declarations
    namespace detail
    {
        class UriImpl;
    }
    
    class Uri
    {
        public:
            //Friends
            
            //Definitions
            
            //Constructors
            Uri( const std::string& value );
            
            Uri( const Uri& original );
            
            Uri( const detail::UriImpl& implementation );
            
            virtual ~Uri( void );
            
            //Functionality
            std::string to_string( void ) const;
            
            std::string to_native_path( void ) const;
            
            static Uri parse( const std::string& value );
            
            static std::string decode( const Bytes& value );
            
            static std::string decode( const std::string& value );
            
            static std::string decode_parameter( const std::string& value );
            
            static std::string encode( const Bytes& value );
            
            static std::string encode( const std::string& value );
            
            //Getters
            int get_port( void ) const;
            
            std::string get_path( void ) const;
            
            std::string get_query( void ) const;
            
            std::string get_scheme( void ) const;
            
            std::string get_fragment( void ) const;
            
            std::string get_username( void ) const;
            
            std::string get_password( void ) const;
            
            std::string get_authority( void ) const;
            
            //Setters
            
            //Operators
            Uri& operator =( const Uri& rhs );
            
            bool operator <( const Uri& rhs ) const;
            
            bool operator >( const Uri& rhs ) const;
            
            bool operator ==( const Uri& rhs ) const;
            
            bool operator !=( const Uri& rhs ) const;
            
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
            Uri( void );
            
            //Functionality
            
            //Getters
            
            //Setters
            
            //Operators
            
            //Properties
            const std::unique_ptr< detail::UriImpl > m_pimpl;
    };
}

#endif  /* _FRAMEWORK_URI_H */
