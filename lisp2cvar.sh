#!/bin/sh

if [ "$1" == "" ]; then
    echo "Usage: lisp2cvar <varname>"
    exit 1
fi

echo "char *$1 = " > $1.c
sed 's/\\/\\\\/g' < $1.lisp | sed 's/;.*//g' | sed 's/"/\\"/g' | 
    sed 's/$/"/g' | sed 's/^/"/g' | sed 's/%/%%/g' | sed 's/^"[ \t]*"/""/g' |
    grep -v '^""' >> $1.c
echo ';\n' >> $1.c
