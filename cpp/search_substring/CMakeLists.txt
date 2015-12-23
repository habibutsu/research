cmake_minimum_required(VERSION 2.7)

if(UNIX)
    set( CMAKE_CXX_FLAGS  "-std=c++98 -pedantic -pedantic-errors -W -Wall -Wextra -Wconversion" )
endif(UNIX)

add_subdirectory(src)

