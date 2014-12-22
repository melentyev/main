/*
 * Copyright (c) 2013, 2014 Corvusoft
 */

//System Includes

//Project Includes
#include "corvusoft/framework/map.h"
#include "corvusoft/framework/string.h"
#include "corvusoft/framework/detail/http_impl.h"

//External Includes
#include <curl/curl.h>

//System Namespaces
using std::map;
using std::stol;
using std::stod;
using std::string;
using std::to_string;

//Project Namespaces

//External Namespaces

namespace framework
{
    namespace detail
    {
        Http::Response HttpImpl::get( const Http::Request& request )
        {
            Http::Request action = request;
            action.method = "GET";
            
            return perform( action );
        }
        
        Http::Response HttpImpl::put( const Http::Request& request )
        {
            Http::Request action = request;
            action.method = "PUT";
            
            return perform( action );
        }
        
        Http::Response HttpImpl::post( const Http::Request& request )
        {
            Http::Request action = request;
            action.method = "POST";
            
            return perform( action );
        }
        
        Http::Response HttpImpl::destroy( const Http::Request& request )
        {
            Http::Request action = request;
            action.method = "DELETE";
            
            return perform( action );
        }
        
        Http::Response HttpImpl::perform( const Http::Request& request )
        {
            Http::Response response;
            
            curl_global_init( CURL_GLOBAL_ALL );
            
            CURL* curl = curl_easy_init( );
            
            if ( curl )
            {
                curl_easy_setopt( curl, CURLOPT_VERBOSE, 0L );
                
                curl_easy_setopt( curl, CURLOPT_CUSTOMREQUEST, request.method.data( ) );
                
                curl_easy_setopt( curl, CURLOPT_WRITEFUNCTION, &write_body_callback );
                
                curl_easy_setopt( curl, CURLOPT_WRITEDATA, &response );
                
                curl_easy_setopt( curl, CURLOPT_HEADERFUNCTION, &write_headers_callback );
                
                curl_easy_setopt( curl, CURLOPT_WRITEHEADER, &response );
                
                curl_easy_setopt( curl, CURLOPT_URL, request.uri.data( ) );
                
                curl_easy_setopt( curl, CURLOPT_POSTFIELDS, &( request.body )[ 0 ] );
                
                curl_easy_setopt( curl, CURLOPT_HTTP_VERSION, request.version );
                
                struct curl_slist* headers = nullptr;
                
                if ( Map::find_key_ignoring_case( "Content-Length", request.headers ) == request.headers.end( ) )
                {
                    string value = "Content-Length: " + ::to_string( request.body.size( ) );
                    
                    headers = curl_slist_append( headers, value.data( ) );
                }
                
                for ( auto header : request.headers )
                {
                    string value = header.first + ": " + header.second;
                    
                    headers = curl_slist_append( headers, value.data( ) );
                }
                
                curl_easy_setopt( curl, CURLOPT_HTTPHEADER, headers );
                
                CURLcode result = curl_easy_perform( curl );
                
                if ( result not_eq CURLE_OK )
                {
                    fprintf( stderr, "curl_easy_perform() failed: %s\n", curl_easy_strerror( result ) );
                }
                
                curl_easy_cleanup( curl );
            }
            
            return response;
        }
        
        size_t HttpImpl::write_body_callback( void* data, size_t size, size_t nmemb, void* ptr )
        {
            Http::Response* response = static_cast< Http::Response* >( ptr );
            
            auto length = size * nmemb;
            
            response->body = Bytes( static_cast< Byte* >( data ), static_cast< Byte* >( data ) + length );
            
            return length;
        }
        
        size_t HttpImpl::write_headers_callback( void* data, size_t size, size_t nmemb, void* ptr )
        {
            Http::Response* response = static_cast< Http::Response* >( ptr );
            
            auto length = size * nmemb;
            auto response_data = String::trim( string( static_cast< char* >( data ), length ) );
            
            if ( not response_data.empty( ) )
            {
                if ( "HTTP/" == String::uppercase( response_data.substr( 0, 5 ) ) )
                {
                    auto parts = String::split( response_data, ' ' );
                    
                    response->status_code = stol( parts[ 1 ] );
                    response->status_message = String::trim( parts[ 2 ] );
                    
                    parts = String::split( parts[ 0 ], '/' );
                    response->version = stod( parts[ 1 ] );
                }
                else
                {
                    auto header = String::split( response_data, ':' );
                    
                    if ( header.size( ) > 1 )
                    {
                        auto name = String::trim( header[ 0 ] );
                        auto value = String::trim( header[ 1 ] );
                        
                        if ( not name.empty( ) )
                        {
                            response->headers[ name ] = value;
                        }
                    }
                }
            }
            
            return length;
        }
    }
}
