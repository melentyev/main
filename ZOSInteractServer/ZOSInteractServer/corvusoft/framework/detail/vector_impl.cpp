/*
 * Copyright (c) 2013, 2014 Corvusoft
 */

//System Includes
#include <algorithm>

//Project Includes
#include "corvusoft/framework/string.h"
#include "corvusoft/framework/detail/vector_impl.h"

//External Includes

//System Namespaces
using std::vector;
using std::string;
using std::find_if;

//Project Namespaces

//External Namespaces

namespace framework
{
    namespace detail
    {
        bool VectorImpl::contains_value_ignoring_case( const string& key, const vector< string >& container )
        {
            auto identifier = String::lowercase( key );
            
            auto iterator = find_if( container.begin( ), container.end( ), [ &identifier ]( const string & value )
            {
                return ( identifier == String::lowercase( value ) );
            } );
            
            return ( iterator not_eq container.end( ) );
        }
    }
}
