#!/bin/bash

COMPILER=../kf/target/debug/kf
SOURCE=./test_prog.kf
DEST=./test_prog.c

trap ctrl_c INT

function ctrl_c() {
    exit 1
}

while true; do
    if [ $SOURCE -nt $DEST ] || [ $COMPILER -nt $DEST ]
    then
        clear
        echo $(date)
        echo "Running " $COMPILER compile -i $SOURCE -o $DEST 
        $COMPILER compile -i $SOURCE -o $DEST && cat $DEST 
    fi
    sleep 1
done
