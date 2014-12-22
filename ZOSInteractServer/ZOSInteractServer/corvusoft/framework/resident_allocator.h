/*
 * Copyright (c) 2013, 2014 Corvusoft
 */

#ifndef _FRAMEWORK_RESIDENT_ALLOCATOR_H
#define _FRAMEWORK_RESIDENT_ALLOCATOR_H 1

//System Includes
#include <limits>
#include <memory>
#include <cstdlib>
#include <algorithm>
#include <sys/mman.h>

//Project Includes

//External Includes

//System Namespaces

//Project Namespaces

//External Namespaces

namespace framework
{
    //Forward declarations
    
    template< typename T >
    class ResidentAllocator
    {
        public:
            //Friends
            
            //Definitions
            typedef T value_type;
            
            typedef value_type* pointer;
            
            typedef value_type const* const_pointer;
            
            typedef value_type& reference;
            
            typedef value_type const& const_reference;
            
            typedef std::size_t size_type;
            
            typedef std::ptrdiff_t difference_type;
            
            template< class U > struct rebind
            {
                typedef ResidentAllocator< U > other;
            };
            
            //Constructors
            ResidentAllocator( void )
            {
                //n/a
            }
            
            template< class Other >
            ResidentAllocator( const ResidentAllocator< Other >& )
            {
                //n/a
            }
            
            virtual ~ResidentAllocator( void )
            {
                //n/a
            }
            
            //Functionality
            size_type max_size( void ) const
            {
                return std::numeric_limits< unsigned long int >::max( ) / sizeof( T );
            }
            
            pointer address( reference x ) const
            {
                return &x;
            }
            
            const_pointer address( const_reference x ) const
            {
                return &x;
            }
            
            pointer allocate( size_type size, const_pointer /* hint */ = 0 )
            {
                void* memory = calloc( size, size * sizeof( T ) );
                
                if ( not memory )
                {
                    throw std::bad_alloc( );
                }
                
                const int status = mlock( memory, size * sizeof( T ) );
                
                if ( status == -1 )
                {
                    throw std::runtime_error( "Failed to lock memory!" );
                }
                
                return static_cast< pointer >( memory );
            }
            
            void deallocate( pointer memory, size_type size )
            {
                std::fill_n( static_cast< volatile char* >( memory ), size * sizeof( T ), 0x00 );
                
                const int status = munlock( memory, size * sizeof( T ) );
                
                if ( status == -1 )
                {
                    throw std::runtime_error( "Failed to unlock memory!" );
                }
                
                free( memory );
            }
            
            void construct( pointer memory, const const_reference& value )
            {
                new ( memory ) value_type( value );
            }
            
            void destroy( pointer memory )
            {
                memory->~value_type( );
                
                memset( memory, 0x00, sizeof( T ) );
            }
            
            //Getters
            
            //Setters
            
            //Operators
            ResidentAllocator& operator =( const ResidentAllocator& ) = delete;
            
            bool operator ==( const ResidentAllocator& ) const
            {
                return false;
            }
            
            bool operator !=( const ResidentAllocator& ) const
            {
                return false;
            }
            
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
    };
}

#endif  /* _FRAMEWORK_RESIDENT_ALLOCATOR_H */
