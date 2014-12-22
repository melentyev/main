/*
 * Copyright (c) 2013, 2014 Corvusoft
 */

#ifndef _FRAMEWORK_DATE_H
#define _FRAMEWORK_DATE_H 1

//System Includes
#include <string>
#include <chrono>

//Project Includes

//External Includes

//System Namespaces

//Project Namespaces

//External Namespaces

namespace framework
{
    //Forward Declarations
    
    class Date
    {
        public:
            //Friends
            
            //Definitions
            
            //Constructors
            
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
            Date( void ) = delete;
            
            Date( const Date& original ) = delete;
            
            virtual ~Date( void ) = delete;
            
            //Functionality
            
            //Getters
            
            //Setters
            
            //Operators
            Date& operator =( const Date& value ) = delete;
            
            //Properties
    };
}

#endif  /* _FRAMEWORK_DATE_H */
