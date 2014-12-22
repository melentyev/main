/*
 * Copyright (c) 2013, 2014 Corvusoft
 */

#ifndef _FRAMEWORK_DETAIL_RUN_ID_IMPL_H
#define _FRAMEWORK_DETAIL_RUN_ID_IMPL_H 1

//System Includes
#include <chrono>
#include <memory>
#include <string>

//Project Includes
#include "corvusoft/framework/unique_id.h"

//External Includes

//System Namespaces

//Project Namespaces

//External Namespaces

namespace framework
{
    //Forward Declarations
    
    namespace detail
    {
        //Forward Declarations
        
        class RunIdImpl
        {
            public:
                //Friends
                
                //Definitions
                
                //Constructors
                RunIdImpl( void );
                
                RunIdImpl( const RunIdImpl& original );
                
                virtual ~RunIdImpl( void );
                
                //Functionality
                std::string to_string( void ) const;
                
                static RunIdImpl generate( void );
                
                static RunIdImpl parse( const std::string& value );
                
                static bool is_valid( const std::string& value );
                
                //Getters
                UniqueId get_unique_id( void ) const;
                
                std::chrono::time_point< std::chrono::system_clock > get_timestamp( void ) const;
                
                //Setters
                void set_unique_id( const UniqueId& value );
                
                void set_timestamp( const std::chrono::time_point< std::chrono::system_clock >& value );
                
                //Operators
                RunIdImpl& operator =( const RunIdImpl& value );
                
                bool operator <( const RunIdImpl& value ) const;
                
                bool operator >( const RunIdImpl& value ) const;
                
                bool operator ==( const RunIdImpl& value ) const;
                
                bool operator !=( const RunIdImpl& value ) const;
                
                //Properties
                
            protected:
                //Friends
                
                //Definitions
                
                //Constructors
                
                //Functionality
                
                //Getters
                
                //Setters
                
                //Operators
                
                //Properties
                
            private:
                //Friends
                
                //Definitions
                
                //Constructors
                
                //Functionality
                
                //Getters
                
                //Setters
                
                //Operators
                
                //Properties
                UniqueId m_unique_id;
                
                std::chrono::time_point< std::chrono::system_clock > m_timestamp;
        };
    }
}

#endif  /* _FRAMEWORK_DETAIL_RUN_ID_IMPL_H */
