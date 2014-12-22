/*
 * Copyright (c) 2013, 2014 Corvusoft
 */

#ifndef _FRAMEWORK_DETAIL_ISTREAM_IMPL_H
#define _FRAMEWORK_DETAIL_ISTREAM_IMPL_H 1

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
    
    namespace detail
    {
        //Forward Declarations
        
        class IStreamImpl
        {
            public:
                //Friends
                
                //Definitions
                
                //Constructor
                
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
                IStreamImpl( void ) = delete;
            
                IStreamImpl( const IStreamImpl& original ) = delete;
            
                virtual ~IStreamImpl( void ) = delete;
            
                //Functionality
            
                //Getters
            
                //Setters
            
                //Operators
                IStreamImpl& operator =( const IStreamImpl& value ) = delete;
            
                //Properties
        };
    }
}

#endif  /* _FRAMEWORK_DETAIL_ISTREAM_IMPL_H */
