#!/bin/bash

DIR=$PWD
cd $DIR

# Delete any existing cscope files
echo "Deleting existing cscope.out cscope.in.out cscope.po.out"
rm -rf cscope.out cscope.in.out cscope.po.out tags TAGS

# Find files to index
echo "Creating index ..."

find . -name "*.c"    \
    -o -name "*.cpp"  \
    -o -name "*.h"    \
    -o -name "*.cc"   \
    -o -name "*.hpp"  \
    -o -name "*.xml"  \
    -o -name "*.py"   \
    -o -name "*.json" > cscope.files

# Run cscope
cscope -R -b -q -i cscope.files

echo "Indexing files ... done"

# Generate TAGS
echo "Creating tags ..."
etags $(find $PWD -name "*.c"   -print \
    -o -name "*.cpp" -print \
    -o -name "*.h"   -print \
    -o -name "*.cc"  -print \
    -o -name "*.hpp" -print \
    -o -name "*.xml" -print \
    -o -name "*.py"  -print \
    -o -name "*.json" -print)

echo "Creating tags ... done"

# Clean up
rm cscope.files

cd -
exit 0