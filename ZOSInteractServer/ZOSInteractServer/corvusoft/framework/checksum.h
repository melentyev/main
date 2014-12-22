/*
 * Copyright (c) 2013, 2014 Corvusoft
 */

#ifndef _FRAMEWORK_CHECKSUM_H
#define _FRAMEWORK_CHECKSUM_H 1

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
    //Forward Delcarations
    namespace detail
    {
        class ChecksumImpl;
    }
    
    class Checksum
    {
        public:
            //Friends
            
            //Definitions
            
            //Constructors
            Checksum( const std::string& hash );
            
            Checksum( const Checksum& original );
            
            Checksum( const detail::ChecksumImpl& implementation );
            
            virtual ~Checksum( void );
            
            //Functionality
            std::string to_string( void ) const;
            
            static bool is_valid( const std::string& value );
            
            static Checksum generate( const Bytes& value );
            
            static Checksum generate( const std::string& value );
            
            static Checksum parse( const std::string& hash );
            
            //Getters
            
            //Setters
            
            //Operators
            Checksum& operator =( const Checksum& value );
            
            bool operator <( const Checksum& value ) const;
            
            bool operator >( const Checksum& value ) const;
            
            bool operator ==( const Checksum& value ) const;
            
            bool operator !=( const Checksum& value ) const;
            
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
            Checksum( void );
            
            //Functionality
            
            //Getters
            
            //Setters
            
            //Operators
            
            //Properties
            const std::unique_ptr< detail::ChecksumImpl > m_pimpl;
    };
}

#endif  /* _FRAMEWORK_CHECKSUM_H */
