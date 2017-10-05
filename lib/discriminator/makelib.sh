#!/bin/sh

g++ -O2 -Wall -c *.cpp 
ar rcsv lib.a *.o
rm *.o