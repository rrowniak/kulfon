#!/bin/bash

COMPILER=./kf/target/debug/kf
SOURCE=./test/test_prog.kf
DEST=./test/test_prog.c

trap ctrl_c INT

function ctrl_c() {
    exit 1
}

while true; do
    clear
    echo "Running " $COMPILER compile -i $SOURCE -o $DEST 
    $COMPILER compile -i $SOURCE -o $DEST && cat $DEST 
    sleep 1
done
