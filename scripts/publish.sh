#!/bin/bash
set -e

if [ ! -d $DIR/sujet ]
then
    echo "Error: No directory $DIR/sujet (\$DIR/sujet)."
    echo "We are probably not at the right place, abort."
    exit 2
fi

set -x

rm -fR $DIR/sujet
mv sujet/ $DIR/

