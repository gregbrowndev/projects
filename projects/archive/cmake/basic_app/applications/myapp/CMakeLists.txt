project (myapp)

# Sets the path to the sources and headers (the same). This is nice as you can include the headers for this specific project (e.g. if it was a library), using 'set(SOURCES ${myapp_SOURCE_DIR})'
set (PROJECT_SOURCE_DIR ${CMAKE_CURRENT_SOURCE_DIR})
set (PROJECT_INCLUDE_DIR ${CMAKE_CURRENT_SOURCE_DIR})

# Set include paths
include_directories( ${TestCMakeProject_SOURCE_DIR}/toolkits )

# Specify the source files to compile.
#set(SOURCES ./mylib.h ./mylib.cpp)

# Can also automatically generate this with GLOB or GLOB_RECURSE
file(GLOB SOURCES "./*.cpp")

# Compile
add_executable(myapp ${SOURCES})

# Link
target_link_libraries(myapp mylib)
