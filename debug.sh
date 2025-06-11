#!/bin/bash

exec > >(tee debug.log) 2>&1

[ -t 1 ] && clear
dune clean &&
dune fmt &&
dune build &&
OCAMLRUNPARAM=b DEBUG=Y dune exec -- minigen --exhaustive --types --size 7 --count 1
