/*
 * Copyright (c) 2013, 2014 Corvusoft
 */

#ifndef _FRAMEWORK_ISTREAM_H
#define _FRAMEWORK_ISTREAM_H 1

//System Includes
#include <istream>

//Project Includes

//External Includes

//System Namespaces

//Project Namespaces

//External Namespaces

namespace framework
{
    //Forward Declarations
    
    class IStream
    {
        public:
            //Friends
            
            //Definitions
            
            //Constructors
            
            //Functionality
            static char reverse_peek( std::istream& value );
        
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
            IStream( void ) = delete;
        
            IStream( const IStream& original ) = delete;
        
            virtual ~IStream( void ) = delete;
        
            //Functionality
        
            //Getters
        
            //Setters
        
            //Operators
            IStream& operator =( const IStream& value ) = delete;
        
            //Properties
    };
}

#endif  /* _FRAMEWORK_ISTREAM_H */
