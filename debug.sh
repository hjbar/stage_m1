#!/bin/bash

clear

OCAMLRUNPARAM=b DEBUG=Y dune exec -- minigen --types --size 200 --count 1 --seed 42 > debug.log 2>&1
