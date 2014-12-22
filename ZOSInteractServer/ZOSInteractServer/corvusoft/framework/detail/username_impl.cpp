/*
 * Copyright (c) 2013, 2014 Corvusoft
 */

//System Includes
#include <regex>
#include <stdexcept>

//Project Includes
#include "corvusoft/framework/string.h"
#include "corvusoft/framework/detail/username_impl.h"

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
        UsernameImpl::UsernameImpl( void ) : m_username( String::empty )
        {
            //n/a
        }
        
        UsernameImpl::UsernameImpl( const UsernameImpl& original ) : m_username( original.m_username )
        {
            //n/a
        }
        
        UsernameImpl::~UsernameImpl( void )
        {
            //n/a
        }
        
        string UsernameImpl::to_string( void ) const
        {
            return m_username;
        }
        
        bool UsernameImpl::is_valid( const string& value )
        {
            return regex_match( value, regex( "^[a-zA-Z0-9_@\\-\\. ]{6,}$" ) );
        }
        
        void UsernameImpl::set_username( const string& value )
        {
            if ( not is_valid( value ) )
            {
                throw invalid_argument( "Argument is not a valid username: " + value );
            }
            
            m_username = value;
        }
        
        bool UsernameImpl::operator <( const UsernameImpl& value ) const
        {
            return m_username < value.m_username;
        }
        
        bool UsernameImpl::operator >( const UsernameImpl& value ) const
        {
            return m_username > value.m_username;
        }
        
        bool UsernameImpl::operator ==( const UsernameImpl& value ) const
        {
            return m_username == value.m_username;
        }
        
        bool UsernameImpl::operator !=( const UsernameImpl& value ) const
        {
            return m_username not_eq value.m_username;
        }
        
        UsernameImpl& UsernameImpl::operator =( const UsernameImpl& value )
        {
            set_username( value.m_username );
            
            return *this;
        }
    }
}
