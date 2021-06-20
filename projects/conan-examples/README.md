# Conan Projects

## Overview

Install [Conan](http://docs.conan.io/en/latest/installation.html):

```commandline
mkvirutalenv conan --python=python3
pip install conan
```

The Conan system has recently (July 2017) begun migrating its packages
from the old repo to a new currated repo called [conan-centre](https://bintray.com/conan/conan-center).
 Many of the Conan packages are not yet in the conan-centre repository 
but can be found within reputable repos, such as [conan-community](https://bintray.com/conan-community)
and [bincrafters](https://bintray.com/bincrafters/public-conan). 

Add these distribution channels:

```commandline
conan remote add community https://api.bintray.com/conan/conan-community/conan
conan remote add bincrafters https://api.bintray.com/conan/bincrafters/public-conan 
```

## Using Conan

### Install Dependencies

Run:

```commandline
mkdir cmake-build-debug && cd cmake-build-debug
conan install .. -s compiler.libcxx=libstdc++11 --build gtest
```

Options:

* Set install folder: `--install-folder=cmake-build-debug`
* Set build type, e.g.: `-s build_type=Debug`


Notes:

* `-s compiler.libcxx=libstdc++11` setting required to compile GTests with C++11


### Removing Packages from Cache

Remove all (build directories, packages and src):

```bash
conan remove "*" -s -b -p -f
```