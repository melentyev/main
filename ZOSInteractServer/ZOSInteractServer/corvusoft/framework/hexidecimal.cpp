/*
 * Copyright (c) 2013, 2014 Corvusoft
 */

//System Includes

//Project Includes
#include "corvusoft/framework/hexidecimal.h"
#include "corvusoft/framework/detail/hexidecimal_impl.h"

//External Includes

//System Namespaces
using std::string;

//Project Namespaces
using framework::detail::HexidecimalImpl;

//External Namespaces

namespace framework
{
    string Hexidecimal::encode( const Bytes& value )
    {
        return HexidecimalImpl::encode( value );
    }
    
    Bytes Hexidecimal::decode( const string& value )
    {
        return HexidecimalImpl::decode( value );
    }
}
