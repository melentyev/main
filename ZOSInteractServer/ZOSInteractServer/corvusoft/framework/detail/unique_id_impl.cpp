/*
 * Copyright (c) 2013, 2014 Corvusoft
 */

//System Includes
#include <regex>
#include <stdexcept>
#include <uuid/uuid.h>

//Project Includes
#include "corvusoft/framework/string.h"
#include "corvusoft/framework/detail/unique_id_impl.h"

//External Includes

//System Namespaces
using std::regex;
using std::string;
using std::invalid_argument;

//Project Namespaces

//External Namespaces

namespace framework
{
    namespace detail
    {
        UniqueIdImpl::UniqueIdImpl( void ) : m_unique_id( String::empty )
        {
            //n/a
        }
        
        UniqueIdImpl::UniqueIdImpl( const UniqueIdImpl& original ) : m_unique_id( original.m_unique_id )
        {
            //n/a
        }
        
        UniqueIdImpl::~UniqueIdImpl( void )
        {
            //n/a
        }
        
        string UniqueIdImpl::to_string( void ) const
        {
            return m_unique_id;
        }
        
        UniqueIdImpl UniqueIdImpl::generate( void )
        {
            uuid_t uuid = { 0 };
            
            uuid_generate( uuid );
            
            char string_uuid[ 37 ] = { 0 };
            
            uuid_unparse( uuid, string_uuid );
            
            UniqueIdImpl pimpl;
            
            pimpl.m_unique_id = string_uuid;
            
            return pimpl;
        }
        
        bool UniqueIdImpl::is_valid( const string& value )
        {
            return regex_match( value, regex( "^[0-9a-zA-Z]{8,}\\-[0-9a-zA-Z]{4,}\\-[0-9a-zA-Z]{4,}\\-[0-9a-zA-Z]{4,}\\-[0-9a-zA-Z]{12,}$" ) );
        }
        
        void UniqueIdImpl::set_id( const string& value )
        {
            if ( not is_valid( value ) )
            {
                throw invalid_argument( "Argument is not a valid unique id: " + value );
            }
            
            m_unique_id = value;
        }
        
        bool UniqueIdImpl::operator <( const UniqueIdImpl& value ) const
        {
            uuid_t lhs_uuid = { 0 };
            
            uuid_parse( m_unique_id.data( ), lhs_uuid );
            
            uuid_t rhs_uuid = { 0 };
            
            uuid_parse( value.m_unique_id.data( ), rhs_uuid );
            
            return ( uuid_compare( lhs_uuid, rhs_uuid ) < 0 );
        }
        
        bool UniqueIdImpl::operator >( const UniqueIdImpl& value ) const
        {
            uuid_t lhs_uuid = { 0 };
            
            uuid_parse( m_unique_id.data( ), lhs_uuid );
            
            uuid_t rhs_uuid = { 0 };
            
            uuid_parse( value.m_unique_id.data( ), rhs_uuid );
            
            return ( uuid_compare( lhs_uuid, rhs_uuid ) > 0 );
        }
        
        bool UniqueIdImpl::operator ==( const UniqueIdImpl& value ) const
        {
            uuid_t lhs_uuid = { 0 };
            
            uuid_parse( m_unique_id.data( ), lhs_uuid );
            
            uuid_t rhs_uuid = { 0 };
            
            uuid_parse( value.m_unique_id.data( ), rhs_uuid );
            
            return ( uuid_compare( lhs_uuid, rhs_uuid ) == 0 );
        }
        
        bool UniqueIdImpl::operator !=( const UniqueIdImpl& value ) const
        {
            return not ( *this == value );
        }
        
        UniqueIdImpl& UniqueIdImpl::operator =( const UniqueIdImpl& value )
        {
            set_id( value.m_unique_id );
            
            return *this;
        }
    }
}
