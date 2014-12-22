/*
 * Copyright (c) 2013, 2014 Corvusoft
 */

#ifndef _FRAMEWORK_DETAIL_DATE_IMPL_H
#define _FRAMEWORK_DETAIL_DATE_IMPL_H 1

//System Includes
#include <chrono>
#include <string>

//Project Includes

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
        
        class DateImpl
        {
            public:
                //Friends
                
                //Definitions
                
                //Constructor
                
                //Functionality
                static std::chrono::time_point< std::chrono::system_clock > parse( const std::string& value );
                
                static std::string format( const std::chrono::time_point< std::chrono::system_clock >& value );
                
                //Getters
                
                //Setters
                
                //Operators
                
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
                DateImpl( void ) = delete;
                
                DateImpl( const DateImpl& original ) = delete;
                
                virtual ~DateImpl( void ) = delete;
                
                //Functionality
                
                //Getters
                
                //Setters
                
                //Operators
                DateImpl& operator =( const DateImpl& value ) = delete;
                
                //Properties
        };
    }
}

#endif  /* _FRAMEWORK_DETAIL_DATE_IMPL_H */
