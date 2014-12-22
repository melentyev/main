/*
 * Copyright (c) 2013, 2014 Corvusoft
 */

#ifndef _FRAMEWORK_VECTOR_H
#define _FRAMEWORK_VECTOR_H 1

//System Includes
#include <vector>
#include <string>

//Project Includes

//External Includes

//System Namespaces

//Project Namespaces

//External Namespaces

namespace framework
{
    //Forward Declarations
    
    class Vector
    {
        public:
            //Friends
            
            //Definitions
            
            //Constructors
            
            //Functionality
            static bool contains_value_ignoring_case( const std::string& key, const std::vector< std::string >& container );
            
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
            Vector( void ) = delete;
            
            Vector( const Vector& original ) = delete;
            
            virtual ~Vector( void ) = delete;
            
            //Functionality
            
            //Getters
            
            //Setters
            
            //Operators
            Vector& operator =( const Vector& value ) = delete;
            
            //Properties
    };
}

#endif  /* _FRAMEWORK_VECTOR_H */
