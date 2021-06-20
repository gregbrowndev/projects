# Add thirdparty libraries as IMPORTED target
add_library(extlibrary_libA SHARED IMPORTED)
set_property(TARGET extlibrary_libA PROPERTY IMPORTED_LOCATION "${TestCMakeProject_SOURCE_DIR}/lib/liblibA.so")
set_property(TARGET extlibrary_libA PROPERTY INTERFACE_INCLUDE_DIRECTORIES "${TestCMakeProject_SOURCE_DIR}/thirdparty")

# otherlibA dependends on otherlibB in the same thirdparty library
set_property(TARGET extlibrary_libA PROPERTY INTERFACE_LINK_LIBRARIES "${TestCMakeProject_SOURCE_DIR}/lib/liblibB.so")

