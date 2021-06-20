project (myotherlib)

set (PROJECT_SOURCE_DIR ${CMAKE_CURRENT_SOURCE_DIR})
set (PROJECT_INCLUDE_DIR ${CMAKE_CURRENT_SOURCE_DIR})

# Specify the source files to compile. Can also automatically generate this with GLOB or GLOB_RECURSE
#set(SOURCES ./mylib.h ./mylib.cpp)
file(GLOB SOURCES "./*.cpp")

# Specifies to create shared object library 'mylib' using the sources
add_library(myotherlib SHARED ${SOURCES})

#Set the location for library installation -- i.e., /usr/lib in this case
# not really necessary in this example. Use "sudo make install" to apply
# install(TARGETS mylib DESTINATION /usr/lib)
