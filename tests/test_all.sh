#!/bin/bash

SCRIPT_DIR=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)

COMPILER=$SCRIPT_DIR/../kf/target/debug/kf
DEST=$SCRIPT_DIR/test_prog.c
DEST_EXE=$SCRIPT_DIR/test_prog

trap ctrl_c INT

function ctrl_c() {
    exit 1
}
for kf_src in $SCRIPT_DIR/test*.kf; do
    echo "Testing $(basename $kf_src)..."
    $COMPILER compile -i $kf_src -o $DEST
    if [ $? -eq 0 ]; then
        gcc -Wall -Wextra -std=c89 -pedantic -pedantic-errors -Wmissing-prototypes -Wstrict-prototypes -Wold-style-definition $DEST -o $DEST_EXE
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
