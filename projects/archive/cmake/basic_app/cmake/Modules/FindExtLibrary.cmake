# - Try to find ExtLibrary
# Once done, this will define
#
#  EXTLIBRARY_FOUND - system has ExtLibrary
#  EXTLIBRARY_INCLUDE_DIRS - the ExtLibrary include directories
#  EXTLIBRARY_LIBRARIES - link these to use ExtLibrary

#include(LibFindMacros)

set (EXTLIBRARY_ROOT_DIR ${TestCMakeProject_SOURCE_DIR}/thirdparty/extlibrary)
MESSAGE(STATUS "EXTLIBRARY_ROOT_DIR: " ${EXTLIBRARY_ROOT_DIR})

find_path(EXTLIBRARY_INCLUDE_DIR
    NAMES libA.h
    PATHS ${EXTLIBRARY_ROOT_DIR}/libA
    DOC "The Extlibrary include directory"
)

find_library(EXTLIBRARY_LIBRARY 
    NAMES libA
    PATHS ${TestCMakeProject_SOURCE_DIR}/lib
    DOC "The Extlibrary library"
)

include(FindPackageHandleStandardArgs)
# handle the QUIETLY and REQUIRED arguments and set LOGGING_FOUND to TRUE
# if all listed variables are TRUE
find_package_handle_standard_args(EXTLIBRARY DEFAULT_MSG EXTLIBRARY_INCLUDE_DIR EXTLIBRARY_LIBRARY)


if (EXTLIBRARY_FOUND)
    set(EXTLIBRARY_LIBRARIES ${EXTLIBRARY_LIBRARY} )
    set(EXTLIBRARY_INCLUDE_DIRS ${EXTLIBRARY_INCLUDE_DIR} )
endif()

# Tell cmake GUIs to ignore the "local" variables.
mark_as_advanced(EXTLIBRARY_ROOT_DIR EXTLIBRARY_INCLUDE_DIR EXTLIBRARY_LIBRARY)
