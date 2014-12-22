/*
 * Copyright (c) 2013, 2014 Corvusoft
 */

//System Includes

//Project Includes
#include "corvusoft/framework/checksum.h"
#include "corvusoft/framework/detail/checksum_impl.h"

//External Includes

//System Namespaces
using std::string;
using std::unique_ptr;

//Project Namespaces
using framework::detail::ChecksumImpl;

//External Namespaces

namespace framework
{
    Checksum::Checksum( const string& hash ) : m_pimpl( new ChecksumImpl )
    {
        m_pimpl->set_checksum( hash );
    }
    
    Checksum::Checksum( const Checksum& original ) : m_pimpl( new ChecksumImpl( *original.m_pimpl ) )
    {
        //n/a
    }
    
    Checksum::Checksum( const ChecksumImpl& implementation ) : m_pimpl( new ChecksumImpl( implementation ) )
    {
        //n/a
    }
    
    Checksum::~Checksum( void )
    {
        //n/a
    }
    
    string Checksum::to_string( void ) const
    {
        return m_pimpl->to_string( );
    }
    
    bool Checksum::is_valid( const string& value )
    {
        return ChecksumImpl::is_valid( value );
    }
    
    Checksum Checksum::generate( const Bytes& value )
    {
        return ChecksumImpl::generate( value );
    }
    
    Checksum Checksum::generate( const string& value )
    {
        return ChecksumImpl::generate( value );
    }
    
    Checksum Checksum::parse( const string& hash )
    {
        Checksum checksum;
        
        checksum.m_pimpl->set_checksum( hash );
        
        return checksum;
    }
    
    Checksum& Checksum::operator =( const Checksum& value )
    {
        *m_pimpl = *value.m_pimpl;
        
        return *this;
    }
    
    bool Checksum::operator <( const Checksum& value ) const
    {
        return *m_pimpl < *value.m_pimpl;
    }
    
    bool Checksum::operator >( const Checksum& value ) const
    {
        return *m_pimpl > *value.m_pimpl;
    }
    
    bool Checksum::operator ==( const Checksum& value ) const
    {
        return *m_pimpl == *value.m_pimpl;
    }
    
    bool Checksum::operator !=( const Checksum& value ) const
    {
        return *m_pimpl not_eq * value.m_pimpl;
    }
    
    Checksum::Checksum( void ) : m_pimpl( new ChecksumImpl )
    {
        //n/a
    }
}
