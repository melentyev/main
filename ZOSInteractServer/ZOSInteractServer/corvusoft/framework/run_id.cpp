/*
 * Copyright (c) 2013, 2014 Corvusoft
 */

//System Includes

//Project Includes
#include "corvusoft/framework/run_id.h"
#include "corvusoft/framework/unique_id.h"
#include "corvusoft/framework/detail/run_id_impl.h"

//External Includes

//System Namespaces
using std::string;
using std::unique_ptr;
using std::chrono::time_point;
using std::chrono::system_clock;

//Project Namespaces
using framework::detail::RunIdImpl;

//External Namespaces

namespace framework
{
    RunId::RunId( const string& value ) : m_pimpl( new RunIdImpl( RunIdImpl::parse( value ) ) )
    {
        //n/a
    }
    
    RunId::RunId( const RunId& original ) : m_pimpl( new RunIdImpl( *original.m_pimpl ) )
    {
        //n/a
    }
    
    RunId::RunId( const RunIdImpl& implementation ) : m_pimpl( new RunIdImpl( implementation ) )
    {
        //n/a
    }
    
    RunId::~RunId( void )
    {
        //n/a
    }
    
    string RunId::to_string( void ) const
    {
        return m_pimpl->to_string( );
    }
    
    RunId RunId::generate( void )
    {
        return RunIdImpl::generate( );
    }
    
    RunId RunId::parse( const string& value )
    {
        return RunIdImpl::parse( value );
    }
    
    bool RunId::is_valid( const string& value )
    {
        return RunIdImpl::is_valid( value );
    }
    
    UniqueId RunId::get_unique_id( void ) const
    {
        return m_pimpl->get_unique_id( );
    }
    
    time_point< system_clock > RunId::get_timestamp( void ) const
    {
        return m_pimpl->get_timestamp( );
    }
    
    RunId& RunId::operator =( const RunId& value )
    {
        *m_pimpl = *value.m_pimpl;
        
        return *this;
    }
    
    bool RunId::operator <( const RunId& value ) const
    {
        return *m_pimpl < *value.m_pimpl;
    }
    
    bool RunId::operator >( const RunId& value ) const
    {
        return *m_pimpl > *value.m_pimpl;
    }
    
    bool RunId::operator ==( const RunId& value ) const
    {
        return *m_pimpl == *value.m_pimpl;
    }
    
    bool RunId::operator !=( const RunId& value ) const
    {
        return *m_pimpl not_eq * value.m_pimpl;
    }
    
    RunId::RunId( void ) : m_pimpl( new RunIdImpl )
    {
        //n/a
    }
}
