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
find_package(OIS REQUIRED)
include_directories(${OIS_INCLUDE_DIR})

# Set some directories / variables.
set(CMAKE_INSTALL_PREFIX ${CMAKE_SOURCE_DIR})
set(LIBRARY_OUTPUT_PATH ${CMAKE_SOURCE_DIR}/lib)

add_subdirectory(${CMAKE_SOURCE_DIR}/libclois-lane/build)
