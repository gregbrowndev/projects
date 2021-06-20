project (mylib)

# Version 1 - Writing external library dependency directly in this CMakeLists.txt

#include_directories(${toolkits_SOURCE_DIR})
#file(GLOB SOURCES "./*.c++")
#add_library(mylib SHARED ${SOURCES})

# Adding thirdparty library to link line using link_directoryies 
#link_directories("${TestCMakeProject_SOURCE_DIR}/lib")
#target_link_libraries(mylib myotherlib otherlibA)

# OR Add thirdparty libraray to link line using full path
#target_link_libraries(mylib myotherlib "${TestCMakeProject_SOURCE_DIR}/lib/liblibA.so")


# Version 2 - Moving dependency info into IMPORTED target in a separate include file 

# IMPORT target from extlibrary.cmake
include(extlibrary)

include_directories(${toolkits_SOURCE_DIR})
file(GLOB SOURCES "./*.c++")
add_library(mylib SHARED ${SOURCES})

# Link
target_link_libraries(mylib myotherlib extlibrary_libA)


# Version 3 - Using a Find module instead of an IMPORTED target

#find_package(ExtLibrary REQUIRED)

#if (NOT EXTLIBRARY_FOUND)
#    message(FATAL_ERROR "Package EXTLIBRARY not found")
#endif (NOT EXTLIBRARY_FOUND)
#MESSAGE( STATUS "EXTLIBRARY_INCLUDE_DIRS:         " ${EXTLIBRARY_INCLUDE_DIRS} )
#MESSAGE( STATUS "EXTLIBRARY_LIBRARIES:         " ${EXTLIBRARY_LIBRARIES} )

#include_directories(${toolkits_SOURCE_DIR} ${EXTLIBRARY_INCLUDE_DIRS}) 
#file(GLOB SOURCES "./*.c++")
#add_library(mylib SHARED ${SOURCES})

#target_link_libraries(mylib myotherlib ${EXTLIBRARY_LIBRARIES}) 
