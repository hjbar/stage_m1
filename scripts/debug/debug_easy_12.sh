clear &&
dune clean &&
dune fmt &&
dune build &&
OCAMLRUNPARAM=b dune exec -- minihell --show-source --show-constraint --log-solver --show-type --show-typed-term tests.t/poly_easy.test
