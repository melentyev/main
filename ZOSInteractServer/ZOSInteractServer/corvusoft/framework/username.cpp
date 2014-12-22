/*
 * Copyright (c) 2013, 2014 Corvusoft
 */

//System Includes

//Project Includes
#include "corvusoft/framework/username.h"
#include "corvusoft/framework/detail/username_impl.h"

//External Includes

//System Namespaces
using std::string;
using std::unique_ptr;

//Project Namespaces
using framework::detail::UsernameImpl;

//External Namespaces

namespace framework
{
    Username::Username( const string& value ) : m_pimpl( new UsernameImpl )
    {
        m_pimpl->set_username( value );
    }
    
    Username::Username( const Username& original ) : m_pimpl( new UsernameImpl( *original.m_pimpl ) )
    {
        //n/a
    }
    
    Username::Username( const UsernameImpl& implementation ) : m_pimpl( new UsernameImpl( implementation ) )
    {
        //n/a
    }
    
    Username::~Username( void )
    {
        //n/a
    }
    
    string Username::to_string( void ) const
    {
        return m_pimpl->to_string( );
    }
    
    bool Username::is_valid( const string& value )
    {
        return UsernameImpl::is_valid( value );
    }
    
    Username Username::parse( const string& value )
    {
        Username username;
        
        username.m_pimpl->set_username( value );
        
        return username;
    }
    
    Username& Username::operator =( const Username& value )
    {
        *m_pimpl = *value.m_pimpl;
        
        return *this;
    }
    
    bool Username::operator <( const Username& value ) const
    {
        return *m_pimpl < *value.m_pimpl;
    }
    
    bool Username::operator >( const Username& value ) const
    {
        return *m_pimpl > *value.m_pimpl;
    }
    
    bool Username::operator ==( const Username& value ) const
    {
        return *m_pimpl == *value.m_pimpl;
    }
    
    bool Username::operator !=( const Username& value ) const
    {
        return *m_pimpl not_eq * value.m_pimpl;
    }
    
    Username::Username( void ) : m_pimpl( new UsernameImpl )
    {
        //n/a
    }
}
