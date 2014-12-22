/*
 * Copyright (c) 2013, 2014 Corvusoft
 */

//System Includes

//Project Includes
#include "corvusoft/framework/map.h"
#include "corvusoft/framework/detail/map_impl.h"

//External Includes

//System Namespaces
using std::map;
using std::string;
using std::multimap;

//Project Namespaces
using framework::detail::MapImpl;

//External Namespaces

namespace framework
{
    Map::iterator Map::find_key_ignoring_case( const string& key, map< string, string >& container )
    {
        return MapImpl::find_key_ignoring_case( key, container );
    }
    
    Map::const_iterator Map::find_key_ignoring_case( const string& key, const map< string, string >& container )
    {
        return MapImpl::find_key_ignoring_case( key, container );
    }
    
    Map::iterator Map::find_key_ignoring_case( const string& key, multimap< string, string >& container )
    {
        return MapImpl::find_key_ignoring_case( key, container );
    }
    
    Map::const_iterator Map::find_key_ignoring_case( const string& key, const multimap< string, string >& container )
    {
        return MapImpl::find_key_ignoring_case( key, container );
    }
}
