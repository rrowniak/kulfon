#!/bin/bash

COMPILER=../kf/target/debug/kf
SOURCE=./live_run_prog.kf
DEST=./live_run_prog.c
DEST_EXE=./live_run__prog

trap ctrl_c INT

function ctrl_c() {
    exit 1
}

touch $SOURCE

while true; do
    if [ $SOURCE -nt $DEST ] || [ $COMPILER -nt $DEST ]
    then
        clear
        echo $(date)
        echo "Running " $COMPILER compile -i $SOURCE -o $DEST
        $COMPILER compile -i $SOURCE -o $DEST
        if [ $? -eq 0 ]; then
            cat $DEST
            echo
            gcc -ansi -pedantic-errors $DEST -o $DEST_EXE
            if [ $? -eq 0 ]; then
                echo "Output:"
                $DEST_EXE
            fi
        fi
    fi
    sleep 1
done
