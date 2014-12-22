/*
 * Copyright (c) 2013, 2014 Corvusoft
 */

//System Includes
#include <regex>
#include <stdexcept>

//Project Includes
#include "corvusoft/framework/string.h"
#include "corvusoft/framework/detail/checksum_impl.h"

//External Includes
#pragma GCC diagnostic ignored "-Wdeprecated-declarations"
#include <gcrypt.h>
#pragma GCC diagnostic ignored "-Wdeprecated-declarations"

//System Namespaces
using std::regex;
using std::string;
using std::regex_match;
using std::invalid_argument;

//Project Namespaces

//External Namespaces

namespace framework
{
    namespace detail
    {
        ChecksumImpl::ChecksumImpl( void ) : m_checksum( String::empty )
        {
            //n/a
        }
        
        ChecksumImpl::ChecksumImpl( const ChecksumImpl& original ) : m_checksum( original.m_checksum )
        {
            //n/a
        }
        
        ChecksumImpl::~ChecksumImpl( void )
        {
            //n/a
        }
        
        string ChecksumImpl::to_string( void ) const
        {
            return m_checksum;
        }
        
        bool ChecksumImpl::is_valid( const string& value )
        {
            return regex_match( value, regex( "^[0-9a-fA-F]+$" ) );
        }
        
        ChecksumImpl ChecksumImpl::generate( const Bytes& value )
        {
            int hash_size = gcry_md_get_algo_dlen( GCRY_MD_MD5 );
            
            Byte* hash = new Byte[ hash_size ];
            
            gcry_md_hash_buffer( GCRY_MD_MD5, hash, &value[0], value.size( ) );
            
            string data = String::empty;
            
            for ( int index = 0; index < hash_size; index++ )
            {
                char hexidecimal[ 3 ] = { 0 };
                
                snprintf( hexidecimal, 3, "%02x", hash[ index ] );
                
                data.append( hexidecimal );
            }
            
            delete[ ] hash;
            
            ChecksumImpl pimpl;
            
            pimpl.m_checksum = data;
            
            return pimpl;
        }
        
        ChecksumImpl ChecksumImpl::generate( const string& value )
        {
            return generate( Bytes( value.begin( ), value.end( ) ) );
        }
        
        void ChecksumImpl::set_checksum( const string& value )
        {
            if ( not is_valid( value ) )
            {
                throw invalid_argument( "Checksum value supplied is not a valid hash: " + value );
            }
            
            m_checksum = value;
        }
        
        bool ChecksumImpl::operator <( const ChecksumImpl& value ) const
        {
            return m_checksum < value.m_checksum;
        }
        
        bool ChecksumImpl::operator >( const ChecksumImpl& value ) const
        {
            return not ( m_checksum < value.m_checksum );
        }
        
        bool ChecksumImpl::operator ==( const ChecksumImpl& value ) const
        {
            return m_checksum == value.m_checksum;
        }
        
        bool ChecksumImpl::operator !=( const ChecksumImpl& value ) const
        {
            return m_checksum not_eq value.m_checksum;
        }
        
        ChecksumImpl& ChecksumImpl::operator =( const ChecksumImpl& value )
        {
            set_checksum( value.m_checksum );
            
            return *this;
        }
    }
}
