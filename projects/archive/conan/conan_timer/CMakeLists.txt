project(FoundationTimer)
cmake_minimum_required(VERSION 2.8.12)

add_definitions("-std=c++11")

if(EXISTS ${CMAKE_BINARY_DIR}/conanbuildinfo.cmake)
    include(${CMAKE_BINARY_DIR}/conanbuildinfo.cmake)
    conan_basic_setup()
else()
    message(WARNING "The file conanbuildinfo.cmake doesn't exist, you have to run conan install first")
endif()


add_executable(timer timer.cpp)
target_link_libraries(timer ${CONAN_LIBS})
