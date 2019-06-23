cmake_minimum_required(VERSION 3.5)
project(TestCMakeProject)
# defines the root project + settings which can be used in subprojects
# can also define new projects, e.g. project(xml_tools) with its own source root, output dirs, settings, etc.

find_package(Boost COMPONENTS date_time)
if(Boost_FOUND)
	MESSAGE( STATUS "Boost_INCLUDE_DIRS:         " ${Boost_INCLUDE_DIRS} )
	MESSAGE( STATUS "Boost_LIBRARY_DIRS:         " ${Boost_LIBRARY_DIRS} )
	MESSAGE( STATUS "Boost_LIBRARIES:         " ${Boost_LIBRARIES} )
endif()

find_package(Qt5 REQUIRED Core)
if(Qt5_FOUND)
	get_target_property(QtCore_location Qt5::Core LOCATION)
	MESSAGE( STATUS "QtCore_location:         " ${QtCore_location} )
	
	get_target_property(QtCore_include_dirs Qt5::Core INTERFACE_INCLUDE_DIRECTORIES)	
	MESSAGE( STATUS "QtCore_include_dirs:         " ${QtCore_include_dirs} )
	
	get_target_property(QtCore_library_dirs Qt5::Core LIBRARY_DIRS)	
	MESSAGE( STATUS "QtCore_library_dirs:         " ${QtCore_library_dirs} )
	
	get_target_property(QtCore_libraries Qt5::Core INTERFACE_LINK_LIBRARIES)	
	MESSAGE( STATUS "QtCore_libraries:         " ${QtCore_libraries} )
endif()


# Setting source tree wide variables using 'CMAKE_' variations of the variables
# i.e. 'RUNTIME_OUTPUT_DIRECTORY' could be set in a subproject set the libs directory for that project

# Can set this here which applies to all subprojects
set(CMAKE_CXX_STANDARD 11)

# Add paths for custom cmake modules
set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${TestCMakeProject_SOURCE_DIR}/cmake/Modules")

# Specify runtime output directory (.exe)
set (CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin)

# Specify project library output directories (.dll .so .a)
set (CMAKE_LIBRARY_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib)
set (CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib)

# Add c++ source directories to project structure using CMakeLists.txt file in those subdirectories
add_subdirectory(toolkits)
add_subdirectory(applications)
