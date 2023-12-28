#! /usr/bin/env bash

# Execute dune tests with code coverage analysis
# This scripts implements the bypass for sandboxing conflicts between
# (1) dune cram-tests, and
# (2) bisect_ppx code coverage analysis
# as shown by @aantron in https://github.com/ocaml/dune/issues/3884

BROWSER='firefox'

# Clean previous run
find . -name '*.coverage' | xargs rm -f

# Run tests with coverage
BISECT_FILE=`pwd`/bisect \
    dune runtest --force --instrument-with bisect_ppx

# Generate and open report
bisect-ppx-report html
$BROWSER _coverage/index.html
