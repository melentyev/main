/*
 * Copyright (c) 2013, 2014 Corvusoft
 */

//System Includes
#include <stdexcept>
#include <algorithm>

//Project Includes
#include "corvusoft/framework/byte.h"
#include "corvusoft/framework/string.h"
#include "corvusoft/framework/detail/hexidecimal_impl.h"

//External Includes

//System Namespaces
using std::size_t;
using std::string;
using std::lower_bound;
using std::length_error;
using std::invalid_argument;

//Project Namespaces

//External Namespaces

namespace framework
{
    namespace detail
    {
        string HexidecimalImpl::encode( const Bytes& value )
        {
            static const char* character_set = "0123456789ABCDEF";
            
            string encoded;
            
            for ( auto byte : value )
            {
                encoded.push_back( character_set[ byte >> 4 ] );
                encoded.push_back( character_set[ byte & 15 ] );
            }
            
            return encoded;
        }
        
        Bytes HexidecimalImpl::decode( const string& value )
        {
            static const char* character_set = "0123456789ABCDEF";
            
            if ( value.length( ) & 1 )
            {
                throw length_error( "incorrect format" );
            }
            
            Bytes decoded;
            
            for ( size_t index = 0; index < value.length( ); index += 2 )
            {
                char character = value.at( index );
                
                const char* lhs = lower_bound( character_set, character_set + 16, character );
                
                if ( *lhs not_eq character )
                {
                    throw invalid_argument( "incorrect format - non hexidecimal character." );
                }
                
                character = value.at( index + 1 );
                
                const char* rhs = lower_bound( character_set, character_set + 16, character );
                
                if ( *rhs not_eq character )
                {
                    throw invalid_argument( "incorrect format - non hexidecimal character." );
                }
                
                auto byte_1 = ( ( lhs - character_set ) << 4 );
                auto byte_2 = ( rhs - character_set );
                
                decoded.push_back( static_cast< Byte >( byte_1 | byte_2 ) );
            }
            
            return decoded;
        }
    }
}
