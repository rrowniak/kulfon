#!/bin/bash

SCRIPT_DIR=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)

COMPILER=$SCRIPT_DIR/../kf/target/debug/kf
DEST=$SCRIPT_DIR/test_prog.c
DEST_EXE=$SCRIPT_DIR/test_prog

GCC_FLAGS=(
    -std=c89
    -pedantic -pedantic-errors
    -Wall -Wextra -Werror
    -Wwrite-strings
    -Wmissing-prototypes -Wstrict-prototypes -Wold-style-definition
    -Wshadow
    -Wconversion -Wsign-conversion
    -Wformat=2
    -Wundef
    -Wnull-dereference
    -Wcast-align
    -Wstack-protector
    -Wdouble-promotion
    -Wno-unused-variable
)

trap ctrl_c INT

function ctrl_c() {
    exit 1
}
for kf_src in $SCRIPT_DIR/test*.kf; do
    echo "Testing $(basename $kf_src)..."
    $COMPILER compile -i $kf_src -o $DEST
    if [ $? -ne 0 ]; then
        echo "FAIL: compiler error for $kf_src"
        exit 1
    fi
    gcc "${GCC_FLAGS[@]}" $DEST -o $DEST_EXE
    if [ $? -ne 0 ]; then
        echo "FAIL: C compilation error for $kf_src"
        exit 1
    fi
    OUT=$($DEST_EXE)
    EXPECTED=$(head -n1 $kf_src | sed -e 's/\/\///')
    if [[ "$OUT" != "$EXPECTED" ]]; then
        echo "FAIL: expected >$EXPECTED< got >$OUT<"
        exit 1
    fi
done
echo "All tests passed."
