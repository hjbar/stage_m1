#! /bin/bash
set -e

if [ ! -f constrained_generation.opam ]
then
    echo "Error: this script is meant to be run"
    echo "from the root directory of the project:"
    echo
    echo "    bash scripts/extract.sh"
    exit 2
fi

if [ -d sujet ]
then
    echo "Error: a sujet/ directory already exists."
    echo "We don't want to damage its content."
    exit 3
fi

set -x

mkdir sujet
mkdir sujet/bin
mkdir sujet/src
mkdir sujet/src/support
mkdir sujet/tests.t

cp constrained_generation.opam sujet/
cp dune sujet/
cp dune-project sujet/
cp README.md sujet/

ROOT='constrained_generation.opam README.md dune dune-project'
BIN='bin/*.* bin/dune'
SUPPORT='src/support/*.* src/support/dune'
SRC='src/*.* src/dune'
TESTS='tests.t/*.*'

for f in $ROOT $BIN $SUPPORT $SRC $TESTS
do
    sed -f scripts/sanitize.sed $f > sujet/$f
done

set +x

echo "You can now test the generated assignment with"
echo "(cd sujet/ ; dune runtest --root .)"

