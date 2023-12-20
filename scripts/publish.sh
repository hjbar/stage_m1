#!/bin/bash
set -e

TEST_FILE=$DIR/constrained_generation.opam
if [ ! -f $TEST_FILE ]
then
    echo "Error: No file $TEST_FILE (\$DIR/constrained_generation.opam)."
    echo "We are probably not at the right place, abort."
    exit 2
fi

if [ ! -d sujet ]
then
    echo "Error: no sujet/ directory."
    echo "You should run scripts/extract.sh first."
    exit 3
fi

set -x

rm -fR $DIR/*
mv sujet/* $DIR/
rmdir sujet
