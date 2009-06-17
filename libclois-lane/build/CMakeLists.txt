# CMakeLists.txt
#
# author: Erik Winkels (aerique@xs4all.nl)
#
# See the LICENSE file in the Okra root directory for more info.

set(LIBCLOIS-LANE_SOURCE_FILES ../src/libclois-lane.cpp)

add_library(clois-lane ${LIBCLOIS-LANE_SOURCE_FILES})

target_link_libraries(clois-lane ${OIS_LIBRARY_REL})
