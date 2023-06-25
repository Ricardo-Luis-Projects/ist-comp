#!/usr/bin/env bash

# Clean up
rm -f *.test.*

# Print the usage
if [ $# -lt 2 ]; then
    echo "Usage: $(basename $0) <mml compiler> [mml file]+"
    exit 1
fi

# The MML compiler
MMLC=$1

# The rest of the arguments are the files to compile
shift
FILES=$@

# The ld flags
LIB=$ROOT/usr/lib
LDFLAGS="-m elf_i386 -znoexecstack -L$LIB -lrts"

# Prints the variables for debugging
echo "MML Compiler: $MMLC"
echo "Files to compile: $FILES"
echo "LD Flags: $LDFLAGS"

# Compiles the MMLecho
echo
echo "====================="
echo "===== COMPILING ====="
echo "====================="
for FILE in $FILES; do
    echo
    echo "=== Compiling $FILE ==="

    # Checks if the file exists
    if [ ! -f $FILE ]; then
        echo "$FILE does not exist"
        exit 1
    fi

    ASM=$(basename $FILE .mml).test.asm
    OBJ=$(basename $FILE .mml).test.o

    # Compiles the MML and stores stderr to a variable
    ERR=$($MMLC -g $FILE -o $ASM 2>&1)

    # Prints the compiled MML
    cat $ASM | grep "^;" | sed 's/; //g'

    # Checks if ERR is empty
    if [ ! -z "$ERR" ]; then
        echo "Error compiling $FILE"
        echo "$ERR"
        exit 1
    fi

    # Assembling
    yasm -f elf32 $ASM -o $OBJ || exit 1
done

OUT=$(basename $(echo $FILES | cut -d' ' -f1) .mml).test.out

# Linking
echo
echo "=== Linking $OUT ==="

# List of object files
OBJECTS=""
for FILE in $FILES; do
    OBJECTS="$OBJECTS $(basename $FILE .mml).test.o"
done

ld -o $OUT $OBJECTS $LDFLAGS || exit 1

# Executes the program
echo
echo "====================="
echo "====== RUNNING ======"
echo "====================="
./$OUT
