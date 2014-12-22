/*
 * Copyright (c) 2013, 2014 Corvusoft
 */

//System Includes
#include <regex>
#include <stdexcept>

//Project Includes
#include "corvusoft/framework/detail/run_id_impl.h"

//External Includes

//System Namespaces
using std::stol;
using std::regex;
using std::string;
using std::to_string;
using std::unique_ptr;
using std::invalid_argument;
using std::chrono::time_point;
using std::chrono::system_clock;

//Project Namespaces

//External Namespaces

namespace framework
{
    namespace detail
    {
        RunIdImpl::RunIdImpl( void ) : m_unique_id( UniqueId::generate( ) ),
            m_timestamp( system_clock::now( ) )
        {
            //n/a
        }
        
        RunIdImpl::RunIdImpl( const RunIdImpl& original ) : m_unique_id( original.m_unique_id ),
            m_timestamp( original.m_timestamp )
        {
            //n/a
        }
        
        RunIdImpl::~RunIdImpl( void )
        {
            //n/a
        }
        
        string RunIdImpl::to_string( void ) const
        {
            return m_unique_id.to_string( ) + "-" + ::to_string( system_clock::to_time_t( m_timestamp ) );
        }
        
        RunIdImpl RunIdImpl::generate( void )
        {
            return RunIdImpl( );
        }
        
        RunIdImpl RunIdImpl::parse( const string& value )
        {
            if ( not is_valid( value ) )
            {
                throw invalid_argument( "Argument is not a valid runtime identifier: " + value );
            }
            
            RunIdImpl result;
            
            size_t index = value.find_last_of( '-' );
            
            result.set_unique_id( UniqueId::parse( value.substr( 0, index ) ) );
            
            time_t timestamp = stol( value.substr( index + 1 ) );
            
            result.set_timestamp( system_clock::from_time_t( timestamp ) );
            
            return result;
        }
        
        bool RunIdImpl::is_valid( const string& value )
        {
            return regex_match( value, regex( "^[0-9a-zA-Z]{8,}\\-[0-9a-zA-Z]{4,}\\-[0-9a-zA-Z]{4,}\\-[0-9a-zA-Z]{4,}\\-[0-9a-zA-Z]{12,}\\-[0-9]{0,12}$" ) );
        }
        
        UniqueId RunIdImpl::get_unique_id( void ) const
        {
            return m_unique_id;
        }
        
        time_point< system_clock > RunIdImpl::get_timestamp( void ) const
        {
            return m_timestamp;
        }
        
        void RunIdImpl::set_unique_id( const UniqueId& value )
        {
            m_unique_id = value;
        }
        
        void RunIdImpl::set_timestamp( const std::chrono::time_point< std::chrono::system_clock >& value )
        {
            m_timestamp = value;
        }
        
        RunIdImpl& RunIdImpl::operator =( const RunIdImpl& value )
        {
            set_unique_id( value.m_unique_id );
            
            set_timestamp( value.m_timestamp );
            
            return *this;
        }
        
        bool RunIdImpl::operator <( const RunIdImpl& value ) const
        {
            return m_timestamp < value.m_timestamp;
        }
        
        bool RunIdImpl::operator >( const RunIdImpl& value ) const
        {
            return m_timestamp > value.m_timestamp;
        }
        
        bool RunIdImpl::operator ==( const RunIdImpl& value ) const
        {
            return m_unique_id == value.m_unique_id and
                   system_clock::to_time_t( m_timestamp ) == system_clock::to_time_t( value.m_timestamp );
        }
        
        bool RunIdImpl::operator !=( const RunIdImpl& value ) const
        {
            return not ( *this == value );
        }
    }
}
