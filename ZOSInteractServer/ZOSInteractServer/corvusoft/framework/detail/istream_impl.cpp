/*
 * Copyright (c) 2013, 2014 Corvusoft
 */

//System Includes

//Project Includes
#include "corvusoft/framework/detail/istream_impl.h"

//External Includes

//System Namespaces
using std::istream;

//Project Namespaces

//External Namespaces

namespace framework
{
    namespace detail
    {
        char IStreamImpl::reverse_peek( istream& value )
        {
            char previous_byte = 0;
            
            if ( value.gcount( ) not_eq 0 )
            {
                value.unget( );
                
                previous_byte = value.get( );
            }
            
            return previous_byte;
        }
    }
}
