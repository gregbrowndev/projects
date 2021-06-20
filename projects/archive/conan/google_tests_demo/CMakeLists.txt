cmake_minimum_required(VERSION 3.9)
project(conan_project)

set(CMAKE_CXX_STANDARD 11)

if(EXISTS ${CMAKE_BINARY_DIR}/conanbuildinfo.cmake)
    include(${CMAKE_BINARY_DIR}/conanbuildinfo.cmake)
    conan_basic_setup()
else()
    message(WARNING "The file conanbuildinfo.cmake doesn't exist, you have to run conan install first")
endif()

add_executable(conan_project main.cpp)
target_link_libraries(conan_project
        ${CONAN_LIBS}
        )

# Google Tests
add_executable(conan_tests test.cpp)
target_link_libraries(conan_tests
        ${CONAN_LIBS}
        )