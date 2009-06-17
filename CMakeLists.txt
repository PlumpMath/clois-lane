# CMakeLists.txt
#
# author: Erik Winkels (aerique@xs4all.nl)
#
# See the LICENSE file in the clois-lane root directory for more info.

cmake_minimum_required(VERSION 2.6)

project(libclois-lane)

option(BUILD_SHARED_LIBS "build libclois-lane shared" ON)

add_definitions(-g -O0 -Wall)

set(CMAKE_MODULE_PATH "${CMAKE_SOURCE_DIR}/cmake-modules")

# Find the OIS library and add its include files.
# (find-package doesn't work on MinGW/MSYS yet)
if (MINGW)
    include_directories("C:/Software/OgreSDK/include/OIS")
    set(OIS_LIBRARY_REL "C:/Software/OgreSDK/bin/release/OIS.dll")
else (MINGW)
    find_package(OIS REQUIRED)
    include_directories(${OIS_INCLUDE_DIR})
endif (MINGW)

# Set some directories / variables.
set(CMAKE_INSTALL_PREFIX ${CMAKE_SOURCE_DIR})
set(LIBRARY_OUTPUT_PATH ${CMAKE_SOURCE_DIR}/lib)

add_subdirectory(${CMAKE_SOURCE_DIR}/libclois-lane/build)
