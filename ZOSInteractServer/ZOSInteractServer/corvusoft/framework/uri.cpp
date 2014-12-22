/*
 * Copyright (c) 2013, 2014 Corvusoft
 */

//System Includes

//Project Includes
#include "corvusoft/framework/uri.h"
#include "corvusoft/framework/detail/uri_impl.h"

//External Includes

//System Namespaces
using std::string;
using std::unique_ptr;

//Project Namespaces
using framework::detail::UriImpl;

//External Namespaces

namespace framework
{
    Uri::Uri( const string& value ) : m_pimpl( new UriImpl )
    {
        m_pimpl->set_uri( value );
    }
    
    Uri::Uri( const Uri& original ) : m_pimpl( new UriImpl( *original.m_pimpl ) )
    {
        //n/a
    }
    
    Uri::Uri( const UriImpl& implementation ) : m_pimpl( new UriImpl( implementation ) )
    {
        //n/a
    }
    
    Uri::~Uri( void )
    {
        //n/a
    }
    
    string Uri::to_string( void ) const
    {
        return m_pimpl->to_string( );
    }
    
    string Uri::to_native_path( void ) const
    {
        return m_pimpl->to_native_path( );
    }
    
    Uri Uri::parse( const string& value )
    {
        Uri uri;
        
        uri.m_pimpl->set_uri( value );
        
        return uri;
    }
    
    string Uri::decode( const Bytes& value )
    {
        return UriImpl::decode( value );
    }
    
    string Uri::decode( const string& value )
    {
        return UriImpl::decode( value );
    }
    
    string Uri::decode_parameter( const string& value )
    {
        return UriImpl::decode_parameter( value );
    }
    
    string Uri::encode( const Bytes& value )
    {
        return UriImpl::encode( value );
    }
    
    string Uri::encode( const string& value )
    {
        return UriImpl::encode( value );
    }
    
    int Uri::get_port( void ) const
    {
        return m_pimpl->get_port( );
    }
    
    string Uri::get_path( void ) const
    {
        return m_pimpl->get_path( );
    }
    
    string Uri::get_query( void ) const
    {
        return m_pimpl->get_query( );
    }
    
    string Uri::get_scheme( void ) const
    {
        return m_pimpl->get_scheme( );
    }
    
    string Uri::get_fragment( void ) const
    {
        return m_pimpl->get_fragment( );
    }
    
    string Uri::get_username( void ) const
    {
        return m_pimpl->get_username( );
    }
    
    string Uri::get_password( void ) const
    {
        return m_pimpl->get_password( );
    }
    
    string Uri::get_authority( void ) const
    {
        return m_pimpl->get_authority( );
    }
    
    Uri& Uri::operator =( const Uri& rhs )
    {
        *m_pimpl = *rhs.m_pimpl;
        
        return *this;
    }
    
    bool Uri::operator <( const Uri& rhs ) const
    {
        return *m_pimpl < *rhs.m_pimpl;
    }
    
    bool Uri::operator >( const Uri& rhs ) const
    {
        return *m_pimpl > *rhs.m_pimpl;
    }
    
    bool Uri::operator ==( const Uri& rhs ) const
    {
        return *m_pimpl == *rhs.m_pimpl;
    }
    
    bool Uri::operator !=( const Uri& rhs ) const
    {
        return *m_pimpl not_eq * rhs.m_pimpl;
    }
    
    Uri::Uri( void ) : m_pimpl( new UriImpl )
    {
        //n/a
    }
}
