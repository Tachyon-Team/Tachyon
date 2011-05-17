#!/usr/bin/env bash

for testfile in ./ut*.js
do

echo "=============================================================="
echo $testfile:
d8 ../../utility/debug.js \
   ../../utility/iterators.js \
   ../../utility/arrays.js \
   ../../utility/misc.js \
   ../../utility/num.js \
   ../../backend/asm.js \
   ../../backend/x86/asm.js \
   $testfile
echo "=============================================================="

done
