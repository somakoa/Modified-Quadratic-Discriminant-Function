#!/bin/sh

DIRECTORY=`dirname $0`

LIB_FILE=$1

echo "Make $1"
rm -f $LIB_FILE
#g++ -pg -O3 -Wall -c ./src/*.cpp
g++ -pg -O3 -Wall -c $DIRECTORY/`basename $LIB_FILE .a`/*.cpp
#`basename $LIB_FILE .a`
#$DIRECTORY/
ar rcsv $LIB_FILE *.o
rm *.o
