/*
 * Copyright (c) 2013, 2014 Corvusoft
 */

#ifndef _FRAMEWORK_DETAIL_URI_IMPL_H
#define _FRAMEWORK_DETAIL_URI_IMPL_H 1

//System Includes
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
        //Forward Declarations
        
        class UriImpl
        {
            public:
                //Friends
                
                //Definitions
                
                //Constructors
                UriImpl( void );
                
                UriImpl( const std::string& value );
                
                UriImpl( const UriImpl& original );
                
                virtual ~UriImpl( void );
                
                //Functionality
                std::string to_string( void ) const;
                
                std::string to_native_path( void ) const;
                
                static UriImpl parse( const std::string& value );
                
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
                void set_uri( const std::string& value );
                
                //Operators
                UriImpl& operator =( const UriImpl& rhs );
                
                bool operator <( const UriImpl& rhs ) const;
                
                bool operator >( const UriImpl& rhs ) const;
                
                bool operator ==( const UriImpl& rhs ) const;
                
                bool operator !=( const UriImpl& rhs ) const;
                
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
                std::string m_uri;
        };
    }
}

#endif  /* _FRAMEWORK_DETAIL_URI_IMPL_H */
