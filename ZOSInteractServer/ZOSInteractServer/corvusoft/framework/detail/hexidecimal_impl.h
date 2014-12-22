/*
 * Copyright (c) 2013, 2014 Corvusoft
 */

#ifndef _FRAMEWORK_DETAIL_HEXIDECIMAL_IMPL_H
#define _FRAMEWORK_DETAIL_HEXIDECIMAL_IMPL_H 1

//System Includes

//Project Includes
#include "corvusoft/framework/bytes.h"
#include "corvusoft/framework/hexidecimal.h"

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
        
        class HexidecimalImpl
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
                HexidecimalImpl( void ) = delete;
                
                HexidecimalImpl( const HexidecimalImpl& original ) = delete;
                
                virtual ~HexidecimalImpl( void ) = delete;
                
                //Functionality
                
                //Getters
                
                //Setters
                
                //Operators
                HexidecimalImpl& operator =( const HexidecimalImpl& value ) = delete;
                
                //Properties
        };
    }
}

#endif  /* _FRAMEWORK_DETAIL_HEXIDECIMAL_IMPL_H */
