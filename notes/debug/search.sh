#!/bin/bash

[ -t 1 ] && clear

for i in $(seq 0 10000); do
  echo "i=$i"
  echo ""

  OCAMLRUNPARAM=b dune exec -- minigen --types --size 10 --count 500 --seed $i > debug.log 2>&1
done
