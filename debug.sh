clear &&
dune clean &&
dune fmt &&
dune build &&
OCAMLRUNPARAM=b dune exec -- minihell --show-type tests.t/poly1.test
