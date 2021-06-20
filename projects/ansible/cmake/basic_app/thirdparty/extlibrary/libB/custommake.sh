#! /bin/bash

g++ -fPIC -c libB.c++
g++ -shared -o liblibB.so libB.o
mv ./liblibB.so ../../../lib
