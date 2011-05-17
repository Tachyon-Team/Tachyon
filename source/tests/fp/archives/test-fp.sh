if [ $# -ne 1 ]
then
    echo "Usage: `basename $0` file"
    exit 1
fi

d8 ../../utility/debug.js \
   ../../utility/iterators.js \
   ../../utility/arrays.js \
   ../../utility/misc.js \
   ../../utility/num.js \
   ../../backend/asm.js \
   ../../backend/x86/asm.js \
   $1
