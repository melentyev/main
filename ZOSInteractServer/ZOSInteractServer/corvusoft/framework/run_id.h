/*
 * Copyright (c) 2013, 2014 Corvusoft
 */

#ifndef _FRAMEWORK_RUN_ID_H
#define _FRAMEWORK_RUN_ID_H 1

//System Includes
#include <chrono>
#include <memory>
#include <string>

//Project Includes

//External Includes

//System Namespaces

//Project Namespaces

//External Namespaces

namespace framework
{
    //Forward Delcarations
    class UniqueId;
    
    namespace detail
    {
        class RunIdImpl;
    }
    
    class RunId
    {
        public:
            //Friends
            
            //Definitions
            
            //Constructors
            RunId( const std::string& value );
            
            RunId( const RunId& original );
            
            RunId( const detail::RunIdImpl& implementation );
            
            virtual ~RunId( void );
            
            //Functionality
            std::string to_string( void ) const;
            
            static RunId generate( void );
            
            static RunId parse( const std::string& value );
            
            static bool is_valid( const std::string& value );
            
            //Getters
            UniqueId get_unique_id( void ) const;
            
            std::chrono::time_point< std::chrono::system_clock > get_timestamp( void ) const;
            
            //Setters
            
            //Operators
            RunId& operator =( const RunId& value );
            
            bool operator <( const RunId& value ) const;
            
            bool operator >( const RunId& value ) const;
            
            bool operator ==( const RunId& value ) const;
            
            bool operator !=( const RunId& value ) const;
            
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
            RunId( void );
            
            //Functionality
            
            //Getters
            
            //Setters
            
            //Operators
            
            //Properties
            const std::unique_ptr< detail::RunIdImpl > m_pimpl;
    };
}

#endif  /* _FRAMEWORK_RUN_ID_H */
