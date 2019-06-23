#! /bin/bash

g++ -fPIC -I ../ -c libA.c++
g++ -shared -o liblibA.so libA.o
mv ./liblibA.so ../../../lib
