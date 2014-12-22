/*
 * Copyright (c) 2013, 2014 Corvusoft
 */

//System Includes
#include <regex>
#include <vector>
#include <sstream>
#include <iostream>
#include <stdexcept>

//Project Includes
#include "corvusoft/framework/byte.h"
#include "corvusoft/framework/string.h"
#include "corvusoft/framework/hash_algorithm.h"
#include "corvusoft/framework/detail/password_impl.h"

//External Includes
#pragma GCC diagnostic ignored "-Wdeprecated-declarations" //See issue tracker #3
#include <gcrypt.h>
#pragma GCC diagnostic ignored "-Wdeprecated-declarations"

//System Namespaces
using std::stoi;
using std::regex;
using std::vector;
using std::string;
using std::getline;
using std::to_string;
using std::istringstream;
using std::runtime_error;
using std::invalid_argument;

//Project Namespaces

//External Namespaces

namespace framework
{
    namespace detail
    {
        PasswordImpl::PasswordImpl( void ) : m_hash( String::empty )
        {
            //n/a
        }
        
        PasswordImpl::PasswordImpl( const PasswordImpl& original ) : m_hash( original.m_hash )
        {
            //n/a
        }
        
        PasswordImpl::~PasswordImpl( void )
        {
            m_hash.erase( );
        }
        
        string PasswordImpl::to_string( void ) const
        {
            return m_hash;
        }
        
        PasswordImpl PasswordImpl::generate( const ResidentString& cleartext )
        {
            return generate( cleartext, generate_salt( ), HashAlgorithm::SHA256 );
        }
        
        PasswordImpl PasswordImpl::generate( const ResidentString& cleartext, const string& salt )
        {
            return generate( cleartext, salt, HashAlgorithm::SHA256 );
        }
        
        PasswordImpl PasswordImpl::generate( const ResidentString& cleartext, const string& salt, const HashAlgorithm algorithm )
        {
            if ( cleartext.length( ) < 8 )
            {
                throw invalid_argument( "Supplied value fails to satisfy the required password strength. Must at least eight characters long." );
            }
            
            if ( salt.length( ) < 8 )
            {
                throw invalid_argument( "Supplied value fails to satisfy the required salt strength. Must at least eight characters long." );
            }
            
            //Check supplied algorithm is available.
            static const char* hash_map[ ] =
            {
                "none",
                "md4",
                "md5",
                "haval",
                "rmd160",
                "tiger",
                "tiger1",
                "tiger2",
                "sha1",
                "sha224",
                "sha256",
                "sha384",
                "sha512",
                "whirlpool",
                "crc32",
                "crc32_rfc1510",
                "crc24_rfc2440"
            };
            
            int algorithm_code = gcry_md_map_name( hash_map[ algorithm ] );
            
            if ( algorithm_code == 0 )
            {
                throw invalid_argument( "Unknown hash algorithm specified." );
            }
            
            //Create a new digest handle.
            gcry_md_hd_t handle = { 0 };
            
            gcry_error_t status = gcry_md_open( &handle, algorithm_code, GCRY_MD_FLAG_SECURE | GCRY_MD_FLAG_HMAC );
            
            if ( status not_eq 0 )
            {
                throw runtime_error( "Failed to generate password: " + string( gcry_strsource( status ) ) + " - " + string( gcry_strerror( status ) ) );
            }
            
            //Set HMAC key value.
            status = gcry_md_setkey( handle, salt.data( ), salt.length( ) );
            
            if ( status not_eq 0 )
            {
                throw runtime_error( "Failed to generate password: " + string( gcry_strsource( status ) ) + " - " + string( gcry_strerror( status ) ) );
            }
            
            //Writer cleartext content.
            gcry_md_write( handle, cleartext.data( ), cleartext.length( ) );
            
            gcry_md_final( handle );
            
            //Read hashed result.
            Byte* hash = gcry_md_read( handle, algorithm_code );
            
            //Create string safe hash.
            string result = ::to_string( algorithm ) + "$" + salt + "$";
            
            int hash_size = gcry_md_get_algo_dlen( algorithm_code );
            
            for ( int index = 0; index < hash_size; index++ )
            {
                char hexidecimal[ 3 ] = { 0 };
                
                snprintf( hexidecimal, 3, "%02x", hash[ index ] );
                
                result.append( hexidecimal );
            }
            
            gcry_md_close( handle );
            
            PasswordImpl pimpl;
            
            pimpl.m_hash = result;
            
            return pimpl;
        }
        
        void PasswordImpl::set_password( const string& value )
        {
            bool valid = regex_match( value, regex( "^[0-9]*\\$[a-zA-Z0-9]*\\$[a-zA-Z0-9]*$" ) );
            
            if ( not valid )
            {
                throw invalid_argument( "Supplied value is not a valid password hash" );
            }
            
            m_hash = value;
        }
        
        PasswordImpl& PasswordImpl::operator =( const PasswordImpl& value )
        {
            m_hash = value.m_hash;
            
            return *this;
        }
        
        bool PasswordImpl::operator ==( const PasswordImpl& value ) const
        {
            return m_hash == value.m_hash;
        }
        
        bool PasswordImpl::operator !=( const PasswordImpl& value ) const
        {
            return m_hash not_eq value.m_hash;
        }
        
        bool PasswordImpl::operator ==( const ResidentString& cleartext ) const
        {
            istringstream stream( m_hash );
            
            vector< string > segments;
            
            while ( not stream.eof( ) )
            {
                string segment = String::empty;
                
                getline( stream, segment, '$' );
                
                segments.push_back( segment );
            }
            
            HashAlgorithm algorithm = static_cast< HashAlgorithm >( stoi( segments[ 0 ] ) );
            
            string salt = segments[ 1 ];
            
            PasswordImpl password = generate( cleartext, salt, algorithm );
            
            return *this == password;
        }
        
        bool PasswordImpl::operator !=( const ResidentString& cleartext ) const
        {
            return not ( *this == cleartext );
        }
        
        string PasswordImpl::generate_salt( string::size_type size )
        {
            static const char range[ ] = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz";
            
            string salt = String::empty;
            
            srand( time( nullptr ) );
            
            for ( size_t index = 0; index < size; ++index )
            {
                salt += range[ rand( ) % ( sizeof( range ) - 1 ) ];
            }
            
            return salt;
        }
    }
}
