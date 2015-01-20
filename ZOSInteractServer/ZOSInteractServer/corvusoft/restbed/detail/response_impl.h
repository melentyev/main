/*
 * Copyright (c) 2013, 2014 Corvusoft
 */

#ifndef _RESTBED_DETAIL_RESPONSE_IMPL_H
#define _RESTBED_DETAIL_RESPONSE_IMPL_H 1

//System Includes
#include <map>
#include <string>

//Project Includes

//External Includes
//#include <corvusoft/framework/bytes>
#include "lib_bridge.h"

//System Namespaces

//Project Namespaces

//External Namespaces

namespace restbed
{
    //Forward Declarations
    namespace detail
    {
        //Forward Declarations
        
        class ResponseImpl
        {
            public:
                //Friends
                
                //Definitions
                
                //Constructors
                ResponseImpl( void );
                
                ResponseImpl( const ResponseImpl& original );
                
                virtual ~ResponseImpl( void );
                
                //Functionality
                framework::Bytes to_bytes( void ) const;
                
                //Getters
                framework::Bytes get_body( void ) const;
            
                double get_version( void ) const;
            
                int get_status_code( void ) const;
            
                std::string get_status_message( void ) const;
            
                std::string get_header( const std::string& name ) const;
                
                std::map< std::string, std::string > get_headers( void ) const;
                
                //Setters
                void set_body( const framework::Bytes& value );
                
                void set_body( const std::string& value );
            
                void set_version( const double value );
            
                void set_status_code( const int value );
            
                void set_status_message( const std::string& value );
                
                void set_header( const std::string& name, const std::string& value );
                
                void set_headers( const std::map< std::string, std::string >& values );
                
                //Operators
                bool operator <( const ResponseImpl& value ) const;
                
                bool operator >( const ResponseImpl& value ) const;
                
                bool operator ==( const ResponseImpl& value ) const;
                
                bool operator !=( const ResponseImpl& value ) const;
                
                ResponseImpl& operator =( const ResponseImpl& value );
                
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
                bool has_header( const std::string& name ) const;
                
                std::string generate_status_section( void ) const;
                
                std::string generate_header_section( void ) const;
                
                std::string generate_default_date_header( void ) const;
                
                std::string generate_default_server_header( void ) const;
                
                std::string generate_default_connection_header( void ) const;
                
                std::string generate_default_content_type_header( void ) const;
                
                std::string generate_default_content_length_header( void ) const;
                
                //Getters
                
                //Setters
                
                //Operators
                
                //Properties
                framework::Bytes m_body;
            
                double m_version;
            
                int m_status_code;
            
                std::string m_status_message;
            
                std::map< std::string, std::string > m_headers;
        };
    }
}

#endif  /* _RESTBED_DETAIL_RESPONSE_IMPL_H */
