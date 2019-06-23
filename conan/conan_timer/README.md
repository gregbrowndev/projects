# example-poco-timer
Example timer with POCO C++ libraries installed with conan C and C++ 
package manager.


## Using CLion

Build Conan Dependencies:

```commandline
conan install . -s build_type=Debug --install-folder=cmake-build-debug --build Poco
```

Note the `--build Poco` option was required as I was getting an error 
that a prebuild package could not be found.

## Notes

When`conan install` is executed, a file named *conanbuildinfo.cmake* is created. 
This file manages the project's requirements.

> We can include conanbuildinfo.cmake in our project’s CMakeLists.txt 
to manage our requirements. The inclusion of conanbuildinfo.cmake doesn’t 
alter cmake environment at all, it just provides `CONAN_` variables 
and some useful macros.