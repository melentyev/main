/*
 * Copyright (c) 2013, 2014 Corvusoft
 */

#ifndef _FRAMEWORK_MAP_H
#define _FRAMEWORK_MAP_H 1

//System Includes
#include <map>
#include <string>

//Project Includes

//External Includes

//System Namespaces

//Project Namespaces

//External Namespaces

namespace framework
{
    //Forward Declarations
    
    class Map
    {
        public:
            //Friends
            
            //Definitions
            typedef std::map< std::string, std::string >::iterator iterator;
            
            typedef std::map< std::string, std::string >::const_iterator const_iterator;
            
            //Constructors
            
            //Functionality
            static iterator find_key_ignoring_case( const std::string& key, std::map< std::string, std::string >& container );
            
            static const_iterator find_key_ignoring_case( const std::string& key, const std::map< std::string, std::string >& container );
            
            static iterator find_key_ignoring_case( const std::string& key, std::multimap< std::string, std::string >& container );
            
            static const_iterator find_key_ignoring_case( const std::string& key, const std::multimap< std::string, std::string >& container );
            
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
            Map( void ) = delete;
            
            Map( const Map& original ) = delete;
            
            virtual ~Map( void ) = delete;
            
            //Functionality
            
            //Getters
            
            //Setters
            
            //Operators
            Map& operator =( const Map& value ) = delete;
            
            //Properties
    };
}

#endif  /* _FRAMEWORK_MAP_H */
