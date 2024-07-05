#!/bin/bash

COMPILER=../kf/target/debug/kf
DEST=./test_prog.c
DEST_EXE=./test_prog

trap ctrl_c INT

function ctrl_c() {
    exit 1
}
for kf_src in test*.kf; do
    echo "Testing $kf_src..."
    $COMPILER compile -i $kf_src -o $DEST
    if [ $? -eq 0 ]; then
        gcc -ansi -pedantic-errors $DEST -o $DEST_EXE
        if [ $? -eq 0 ]; then
            OUT=$($DEST_EXE)
            EXPECTED=$(head -n1 $kf_src | sed -e 's/\/\///')
            if [[ "$OUT" != "$EXPECTED" ]]; then
                echo "Test failed: expected >$EXPECTED< != got >$OUT<"
                exit 1
            fi
        fi
    fi
    sleep 1
done
