/*
 * Copyright (c) 2013, 2014 Corvusoft
 */

//System Includes

//Project Includes
#include "corvusoft/framework/istream.h"
#include "corvusoft/framework/detail/istream_impl.h"

//External Includes

//System Namespaces
using std::istream;

//Project Namespaces
using framework::detail::IStreamImpl;

//External Namespaces

namespace framework
{
    char IStream::reverse_peek( istream& value )
    {
        return IStreamImpl::reverse_peek( value );
    }
}
