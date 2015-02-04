#!/bin/bash

DIR=/project/swbuild169/emehhim/spider
cd $DIR
   
# Delete any existing cscope files
echo "Deleting existing cscope.out cscope.in.out cscope.po.out TAGS"
rm -rf cscope.out cscope.in.out cscope.po.out tags TAGS

#Find files to index
echo "Creating index ..."
find $DIR -name "*.c" -o -name "*.cpp" -o -name "*.h"   \
    -o -name "*.hpp"  -o -name "*.xml"  \
    -o -name "*.py" -o -name "*.json" | grep -v "dse" > cscope.files

cscope -R -b -q -i cscope.files
rm cscope.files
echo "Indexing files ... done"

# Create Tags
echo "Creating tags ..."
(cd $DIR && find . -type f -iname "*.[ch]" -o -name "*.py" | xargs etags -a)
echo "Creating tags ... done"
exit 0
