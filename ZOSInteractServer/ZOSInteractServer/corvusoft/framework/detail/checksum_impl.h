/*
 * Copyright (c) 2013, 2014 Corvusoft
 */

#ifndef _FRAMEWORK_DETAIL_CHECKSUM_IMPL_H
#define _FRAMEWORK_DETAIL_CHECKSUM_IMPL_H 1

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
        //Forward Declarations
        
        class ChecksumImpl
        {
            public:
                //Friends
                
                //Definitions
                
                //Constructors
                ChecksumImpl( void );
                
                ChecksumImpl( const ChecksumImpl& original );
                
                virtual ~ChecksumImpl( void );
                
                //Functionality
                std::string to_string( void ) const;
                
                static bool is_valid( const std::string& value );
                
                static ChecksumImpl generate( const Bytes& value );
                
                static ChecksumImpl generate( const std::string& value );
                
                //Getters
                
                //Setters
                void set_checksum( const std::string& value );
                
                //Operators
                bool operator <( const ChecksumImpl& value ) const;
                
                bool operator >( const ChecksumImpl& value ) const;
                
                bool operator ==( const ChecksumImpl& value ) const;
                
                bool operator !=( const ChecksumImpl& value ) const;
                
                ChecksumImpl& operator =( const ChecksumImpl& value );
                
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
                std::string m_checksum;
        };
    }
}

#endif  /* _FRAMEWORK_DETAIL_CHECKSUM_IMPL_H */
