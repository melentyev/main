/*
 * Copyright (c) 2013, 2014 Corvusoft
 */

#ifndef _FRAMEWORK_HEXIDECIMAL_H
#define _FRAMEWORK_HEXIDECIMAL_H 1

//System Includes
#include <string>

//Project Includes
#include <corvusoft/framework/bytes>

//External Includes

//System Namespaces

//Project Namespaces

//External Namespaces

namespace framework
{
    //Forward Delcarations
    
    class Hexidecimal
    {
        public:
            //Friends
            
            //Definitions
            
            //Constructors
            
            //Functionality
            static std::string encode( const Bytes& value );
            
            static Bytes decode( const std::string& value );
            
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
            Hexidecimal( void ) = delete;
            
            Hexidecimal( const Hexidecimal& original ) = delete;
            
            virtual ~Hexidecimal( void ) = delete;
            
            //Functionality
            
            //Getters
            
            //Setters
            
            //Operators
            Hexidecimal& operator =( const Hexidecimal& value ) = delete;
            
            //Properties
    };
}

#endif  /* _FRAMEWORK_HEXIDECIMAL_H */
