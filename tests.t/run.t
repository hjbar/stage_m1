# TL;DR

To run the tests, run

```
dune runtest
```

from the root of the project directory. If this outputs
nothing, the testsuite passes. If this outputs a diff, it
means that there is a mismatch between the recorded/reference
output and the behavior of your program.

To *promote* the tests outputs (that is, to modify the reference
output to match the current behavior of your program), run

```
dune runtest
dune promote
```

When you submit your project, please check that `dune runtest` does
not produce a diff -- the recorded output should match your
program. If some outputs are wrong / not what you would expect, please
explain this in the present file.


# Intro

This file is a "dune cram test" as explained at
> https://dune.readthedocs.io/en/stable/tests.html#cram-tests

The idea is to write 2-indented command lines prefixed by
a dollar sign. The tool will run the command and check that
the output corresponds to the output recorded in the file.

  $ echo example
  example

To run the tests, just run `dune runtest` at the root of the
project. This will show you a diff between the observed
output and the recorded output of the test -- we consider
that the test 'passes' if the diff is empty.
In particular, if you run `dune runtest` and you see no
output, this is good! It means there was no change in the
test output.

If you think that the new output is better than the previous
output, run `dune promote`; dune will rewrite the recorded
outputs to match the observed outputs. (You can also modify
outputs by hand but this is more cumbersome.)

It is totally okay to have some test outputs recorded in
your repository that are known to be broken -- because there
is a bug, or some feature is not documented yet. Feel free
to use the free-form comments in run.t to mention explicitly
that the output is broken. (But then please remember, as the
output changes in the future, to also update your comments.)


# The tests

The tests below use the `minihell` program defined in
../bin/minihell.ml, called on the *.test files stored in the
present directory. If you want to add new tests, just add
new test files and then new commands below to exercise them.

`minihell` takes untyped programs as input and will
type-check and elaborate them. It can show many things
depending on the input flags passed. By default we ask
`minihell` to repeat the source file (to make the recorded
output here more pleasant to read) and to show the generated
constraint. It will also show the result type and the
elaborated term.

  $ FLAGS="--show-source --show-constraint --show-type --show-typed-term"

Remark: You can call minihell from the command-line yourself
by using either
> dune exec bin/minihell.exe -- <arguments>
or
> dune exec minihell -- <arguments>
(The latter short form, used in the tests below, is available thanks
to the bin/dune content.)




## Simple tests

`id_poly` is just the polymorphic identity.

  $ minihell $FLAGS id_poly.test
  Input term:
    lambda x. x
  
  Generated constraint:
    ∃.
      let ?scheme_final_scheme : ?final_term =
      ∃?x ?wt (?warr = ?x -> ?wt). ?final_term = ?warr ∧ ?wt = ?x ∧ decode ?x
      in
      decode_scheme ?scheme_final_scheme
  
  Inferred type:
    ∀α. α -> α
  
  Elaborated term:
    lambda (x : α). x
  




  $ minihell $FLAGS id_poly2.test
  Input term:
    lambda y. let id = lambda x. x in id y
  
  Generated constraint:
    ∃.
      let ?scheme_final_scheme : ?final_term =
      ∃?y ?wt (?warr = ?y -> ?wt).
        ?final_term = ?warr
        ∧ (∃.
          let ?scheme_s : ?id =
          ∃?x ?wt/2 (?warr/1 = ?x -> ?wt/2).
            ?id = ?warr/1 ∧ ?wt/2 = ?x ∧ decode ?x
          in
          (∃.
            (∃?wu (?wt/1 = ?wu -> ?wt). ?scheme_s ≤ ?wt/1 ∧ ?wu = ?y)
            ∧ decode_scheme ?scheme_s))
        ∧ decode ?y
      in
      decode_scheme ?scheme_final_scheme
  
  Inferred type:
    ∀α. α -> α
  
  Elaborated term:
    lambda (y : α). let (id : β -> β) = lambda (x : β). x in id y
  




`id_int` is the monomorphic identity on the type `int`. Note
that we have not implemented support for a built-in `int`
type, this is just an abstract/rigid type variable: `Constr
(Var ...)` at type `STLC.ty`.

  $ minihell $FLAGS id_int.test
  Input term:
    lambda x. (x : int)
  
  Generated constraint:
    ∃.
      let ?scheme_final_scheme : ?final_term =
      ∃?x ?wt (?warr = ?x -> ?wt).
        ?final_term = ?warr
        ∧ (∃(?int = int). ?int = ?wt ∧ ?int = ?x)
        ∧ decode ?x
      in
      decode_scheme ?scheme_final_scheme
  
  Inferred type:
    int -> int
  
  Elaborated term:
    lambda (x : int). (x : int)
  




  $ minihell $FLAGS let_easy.test
  Input term:
    let x = lambda y. y in x
  
  Generated constraint:
    ∃.
      let ?scheme_final_scheme : ?final_term =
      ∃.
        let ?scheme_s : ?x =
        ∃?y ?wt (?warr = ?y -> ?wt). ?x = ?warr ∧ ?wt = ?y ∧ decode ?y
        in
        (∃. ?scheme_s ≤ ?final_term ∧ decode_scheme ?scheme_s)
      in
      decode_scheme ?scheme_final_scheme
  
  Inferred type:
    α -> α
  
  Elaborated term:
    let (x : β -> β) = lambda (y : β). y in x
  






## Examples with products

  $ minihell $FLAGS curry.test
  Input term:
    lambda f. lambda x. lambda y. f (x, y)
  
  Generated constraint:
    ∃.
      let ?scheme_final_scheme : ?final_term =
      ∃?f ?wt (?warr = ?f -> ?wt).
        ?final_term = ?warr
        ∧ (∃?x ?wt/1 (?warr/1 = ?x -> ?wt/1).
          ?wt = ?warr/1
          ∧ (∃?y ?wt/2 (?warr/2 = ?y -> ?wt/2).
            ?wt/1 = ?warr/2
            ∧ (∃?wu (?wt/3 = ?wu -> ?wt/2).
              ?wt/3 = ?f
              ∧ (∃?w1.
                ?w1 = ?x
                ∧ (∃?w2. ?w2 = ?y ∧ (∃(?wprod = {?w1 * ?w2}). ?wu = ?wprod))))
            ∧ decode ?y)
          ∧ decode ?x)
        ∧ decode ?f
      in
      decode_scheme ?scheme_final_scheme
  
  Inferred type:
    ∀β. ∀γ. ∀α. ({γ * β} -> α) -> γ -> β -> α
  
  Elaborated term:
    lambda (f : {γ * β} -> α). lambda (x : γ). lambda (y : β). f (x, y)
  




  $ minihell $FLAGS uncurry.test
  Input term:
    lambda f. lambda p. let (x, y) = p in f x y
  
  Generated constraint:
    ∃.
      let ?scheme_final_scheme : ?final_term =
      ∃?f ?wt (?warr = ?f -> ?wt).
        ?final_term = ?warr
        ∧ (∃?p ?wt/1 (?warr/1 = ?p -> ?wt/1).
          ?wt = ?warr/1
          ∧ (∃?x ?y (?wt/2 = {?x * ?y}).
            decode ?y
            ∧ decode ?x
            ∧ ?wt/2 = ?p
            ∧ (∃?wu (?wt/3 = ?wu -> ?wt/1).
              (∃?wu/1 (?wt/4 = ?wu/1 -> ?wt/3). ?wt/4 = ?f ∧ ?wu/1 = ?x)
              ∧ ?wu = ?y))
          ∧ decode ?p)
        ∧ decode ?f
      in
      decode_scheme ?scheme_final_scheme
  
  Inferred type:
    ∀γ. ∀β. ∀α. (β -> γ -> α) -> {β * γ} -> α
  
  Elaborated term:
    lambda
    (f : β -> γ -> α).
      lambda (p : {β * γ}). let ((x : β), (y : γ)) = p in f x y
  






## Polymorhpism

  $ minihell $FLAGS poly_easy.test
  Input term:
    lambda a. let id = lambda x. x in let r = id a in r
  
  Generated constraint:
    ∃.
      let ?scheme_final_scheme : ?final_term =
      ∃?a ?wt (?warr = ?a -> ?wt).
        ?final_term = ?warr
        ∧ (∃.
          let ?scheme_s : ?id =
          ∃?x ?wt/2 (?warr/1 = ?x -> ?wt/2).
            ?id = ?warr/1 ∧ ?wt/2 = ?x ∧ decode ?x
          in
          (∃.
            (∃.
              let ?scheme_s/1 : ?r =
              ∃?wu (?wt/1 = ?wu -> ?r). ?scheme_s ≤ ?wt/1 ∧ ?wu = ?a
              in
              (∃. ?scheme_s/1 ≤ ?wt ∧ decode_scheme ?scheme_s/1))
            ∧ decode_scheme ?scheme_s))
        ∧ decode ?a
      in
      decode_scheme ?scheme_final_scheme
  
  Inferred type:
    ∀α. α -> α
  
  Elaborated term:
    lambda
    (a : α).
      let (id : β -> β) = lambda (x : β). x in let (r : α) = id a in r
  




  $ minihell $FLAGS poly1.test
  Input term:
    let
    id
    =
    lambda x. x
    in lambda a. lambda b. let l = id a in let r = id b in (l, r)
  
  Generated constraint:
    ∃.
      let ?scheme_final_scheme : ?final_term =
      ∃.
        let ?scheme_s : ?id =
        ∃?x ?wt/4 (?warr/2 = ?x -> ?wt/4).
          ?id = ?warr/2 ∧ ?wt/4 = ?x ∧ decode ?x
        in
        (∃.
          (∃?a ?wt (?warr = ?a -> ?wt).
            ?final_term = ?warr
            ∧ (∃?b ?wt/1 (?warr/1 = ?b -> ?wt/1).
              ?wt = ?warr/1
              ∧ (∃.
                let ?scheme_s/1 : ?l =
                ∃. ∃?wu/1 (?wt/3 = ?wu/1 -> ?l). ?scheme_s ≤ ?wt/3 ∧ ?wu/1 = ?a
                in
                (∃.
                  (∃.
                    let ?scheme_s/2 : ?r =
                    ∃. ∃?wu (?wt/2 = ?wu -> ?r). ?scheme_s ≤ ?wt/2 ∧ ?wu = ?b
                    in
                    (∃.
                      (∃?w1.
                        ?scheme_s/1 ≤ ?w1
                        ∧ (∃?w2.
                          ?scheme_s/2 ≤ ?w2
                          ∧ (∃(?wprod = {?w1 * ?w2}). ?wt/1 = ?wprod)))
                      ∧ decode_scheme ?scheme_s/2))
                  ∧ decode_scheme ?scheme_s/1))
              ∧ decode ?b)
            ∧ decode ?a)
          ∧ decode_scheme ?scheme_s)
      in
      decode_scheme ?scheme_final_scheme
  
  Inferred type:
    ∀α. ∀β. α -> β -> {α * β}
  
  Elaborated term:
    let
    (id : γ -> γ)
    =
    lambda (x : γ). x
    in
      lambda
      (a : α).
        lambda (b : β). let (l : α) = id a in let (r : β) = id b in (l, r)
  





  $ minihell $FLAGS poly2.test
  Input term:
    let
    id
    =
    lambda x. x
    in
      lambda
      a. lambda b. let l = id a in let r = id b in ((l : int), (r : bool))
  
  Generated constraint:
    ∃.
      let ?scheme_final_scheme : ?final_term =
      ∃.
        let ?scheme_s : ?id =
        ∃?x ?wt/4 (?warr/2 = ?x -> ?wt/4).
          ?id = ?warr/2 ∧ ?wt/4 = ?x ∧ decode ?x
        in
        (∃.
          (∃?a ?wt (?warr = ?a -> ?wt).
            ?final_term = ?warr
            ∧ (∃?b ?wt/1 (?warr/1 = ?b -> ?wt/1).
              ?wt = ?warr/1
              ∧ (∃.
                let ?scheme_s/1 : ?l =
                ∃. ∃?wu/1 (?wt/3 = ?wu/1 -> ?l). ?scheme_s ≤ ?wt/3 ∧ ?wu/1 = ?a
                in
                (∃.
                  (∃.
                    let ?scheme_s/2 : ?r =
                    ∃. ∃?wu (?wt/2 = ?wu -> ?r). ?scheme_s ≤ ?wt/2 ∧ ?wu = ?b
                    in
                    (∃.
                      (∃?w1.
                        (∃(?int = int). ?int = ?w1 ∧ ?scheme_s/1 ≤ ?int)
                        ∧ (∃?w2.
                          (∃(?bool = bool). ?bool = ?w2 ∧ ?scheme_s/2 ≤ ?bool)
                          ∧ (∃(?wprod = {?w1 * ?w2}). ?wt/1 = ?wprod)))
                      ∧ decode_scheme ?scheme_s/2))
                  ∧ decode_scheme ?scheme_s/1))
              ∧ decode ?b)
            ∧ decode ?a)
          ∧ decode_scheme ?scheme_s)
      in
      decode_scheme ?scheme_final_scheme
  
  Inferred type:
    ∀α. ∀β. α -> β -> {α * β}
  
  Elaborated term:
    let
    (id : γ -> γ)
    =
    lambda (x : γ). x
    in
      lambda
      (a : α).
        lambda
        (b : β).
          let (l : α) = id a in let (r : β) = id b in ((l : int), (r : bool))
  






## Logging the constraint-solving process

You can ask `minihell` to show how the constraint evolves as
the solver progresses and accumulates more information on
the inference variables.

  $ minihell $FLAGS --log-solver id_int.test
  Input term:
    lambda x. (x : int)
  
  Generated constraint:
    ∃.
      let ?scheme_final_scheme : ?final_term =
      ∃?x ?wt (?warr = ?x -> ?wt).
        ?final_term = ?warr
        ∧ (∃(?int = int). ?int = ?wt ∧ ?int = ?x)
        ∧ decode ?x
      in
      decode_scheme ?scheme_final_scheme
  
  Constraint solving log:
  ∃.
    let ?scheme_final_scheme : ?final_term =
    ∃?x ?wt (?warr = ?x -> ?wt).
      ?final_term = ?warr
      ∧ (∃(?int = int). ?int = ?wt ∧ ?int = ?x)
      ∧ decode ?x
    in
    decode_scheme ?scheme_final_scheme
  ∃.
    let ?scheme_final_scheme : ?final_term =
    ∃?x ?wt (?warr = ?x -> ?wt).
      ?final_term = ?warr
      ∧ (∃(?int = int). ?int = ?wt ∧ ?int = ?x)
      ∧ decode ?x
    in
    decode_scheme ?scheme_final_scheme
  ∃.
    let ?scheme_final_scheme : ?final_term =
    (∃?x.
      ∃?wt (?warr = ?x -> ?wt).
        ?final_term = ?warr
        ∧ (∃(?int = int). ?int = ?wt ∧ ?int = ?x)
        ∧ decode ?x)
    in
    decode_scheme ?scheme_final_scheme
  ∃.
    let ?scheme_final_scheme : ?final_term =
    (∃?x ?wt.
      ∃(?warr = ?x -> ?wt).
        ?final_term = ?warr
        ∧ (∃(?int = int). ?int = ?wt ∧ ?int = ?x)
        ∧ decode ?x)
    in
    decode_scheme ?scheme_final_scheme
  ∃.
    let ?scheme_final_scheme : ?final_term =
    (∃?x ?wt (?warr = ?x -> ?wt).
      ∃.
        ?final_term = ?warr
        ∧ (∃(?int = int). ?int = ?wt ∧ ?int = ?x)
        ∧ decode ?x)
    in
    decode_scheme ?scheme_final_scheme
  ∃.
    let ?scheme_final_scheme : ?final_term =
    (∃?x ?wt (?final_term' = ?x -> ?wt). ?final_term = ?final_term')
    in
    decode_scheme ?scheme_final_scheme
  ∃.
    let ?scheme_final_scheme : ?final_term =
    (∃?x ?wt (?int = int) (?final_term'/1 = ?x -> ?wt).
      ?final_term = ?final_term'/1)
    in
    decode_scheme ?scheme_final_scheme
  ∃.
    let ?scheme_final_scheme : ?final_term =
    (∃?x (?int = int) (?final_term'/2 = ?x -> ?int).
      ?final_term = ?final_term'/2)
    in
    decode_scheme ?scheme_final_scheme
  ∃.
    let ?scheme_final_scheme : ?final_term =
    (∃(?int = int) (?final_term'/3 = ?int -> ?int).
      ?final_term = ?final_term'/3)
    in
    decode_scheme ?scheme_final_scheme
  ∃.
    let ?scheme_final_scheme : ?final_term =
    (∃(?int = int) (?final_term'/4 = ?int -> ?int).
      ?final_term = ?final_term'/4)
    in
    decode_scheme ?scheme_final_scheme
  
  Inferred type:
    int -> int
  
  Elaborated term:
    lambda (x : int). (x : int)
  




 $ minihell $FLAGS --log-solver log_solver.test





## Clash types

  $ minihell $FLAGS --log-solver error.test
  Input term:
    lambda x. (x : int) lambda y. y
  
  Generated constraint:
    ∃.
      let ?scheme_final_scheme : ?final_term =
      ∃?wu (?wt = ?wu -> ?final_term).
        (∃?x ?wt/1 (?warr = ?x -> ?wt/1).
          ?wt = ?warr ∧ (∃(?int = int). ?int = ?wt/1 ∧ ?int = ?x) ∧ decode ?x)
        ∧ (∃?y ?wt/2 (?warr/1 = ?y -> ?wt/2).
          ?wu = ?warr/1 ∧ ?wt/2 = ?y ∧ decode ?y)
      in
      decode_scheme ?scheme_final_scheme
  
  Constraint solving log:
  ∃.
    let ?scheme_final_scheme : ?final_term =
    ∃?wu (?wt = ?wu -> ?final_term).
      (∃?x ?wt/1 (?warr = ?x -> ?wt/1).
        ?wt = ?warr ∧ (∃(?int = int). ?int = ?wt/1 ∧ ?int = ?x) ∧ decode ?x)
      ∧ (∃?y ?wt/2 (?warr/1 = ?y -> ?wt/2).
        ?wu = ?warr/1 ∧ ?wt/2 = ?y ∧ decode ?y)
    in
    decode_scheme ?scheme_final_scheme
  ∃.
    let ?scheme_final_scheme : ?final_term =
    ∃?wu (?wt = ?wu -> ?final_term).
      (∃?x ?wt/1 (?warr = ?x -> ?wt/1).
        ?wt = ?warr ∧ (∃(?int = int). ?int = ?wt/1 ∧ ?int = ?x) ∧ decode ?x)
      ∧ (∃?y ?wt/2 (?warr/1 = ?y -> ?wt/2).
        ?wu = ?warr/1 ∧ ?wt/2 = ?y ∧ decode ?y)
    in
    decode_scheme ?scheme_final_scheme
  ∃.
    let ?scheme_final_scheme : ?final_term =
    (∃?wu.
      ∃(?wt = ?wu -> ?final_term).
        (∃?x ?wt/1 (?warr = ?x -> ?wt/1).
          ?wt = ?warr ∧ (∃(?int = int). ?int = ?wt/1 ∧ ?int = ?x) ∧ decode ?x)
        ∧ (∃?y ?wt/2 (?warr/1 = ?y -> ?wt/2).
          ?wu = ?warr/1 ∧ ?wt/2 = ?y ∧ decode ?y))
    in
    decode_scheme ?scheme_final_scheme
  ∃.
    let ?scheme_final_scheme : ?final_term =
    (∃?wu ?final_term (?wt = ?wu -> ?final_term).
      ∃.
        (∃?x ?wt/1 (?warr = ?x -> ?wt/1).
          ?wt = ?warr ∧ (∃(?int = int). ?int = ?wt/1 ∧ ?int = ?x) ∧ decode ?x)
        ∧ (∃?y ?wt/2 (?warr/1 = ?y -> ?wt/2).
          ?wu = ?warr/1 ∧ ?wt/2 = ?y ∧ decode ?y))
    in
    decode_scheme ?scheme_final_scheme
  ∃.
    let ?scheme_final_scheme : ?final_term =
    (∃?x ?wu ?final_term (?wt = ?wu -> ?final_term).
      ∃.
        (∃?wt/1 (?warr = ?x -> ?wt/1).
          ?wt = ?warr ∧ (∃(?int = int). ?int = ?wt/1 ∧ ?int = ?x) ∧ decode ?x)
        ∧ (∃?y ?wt/2 (?warr/1 = ?y -> ?wt/2).
          ?wu = ?warr/1 ∧ ?wt/2 = ?y ∧ decode ?y))
    in
    decode_scheme ?scheme_final_scheme
  ∃.
    let ?scheme_final_scheme : ?final_term =
    (∃?wt/1 ?x ?wu ?final_term (?wt = ?wu -> ?final_term).
      ∃.
        (∃(?warr = ?x -> ?wt/1).
          ?wt = ?warr ∧ (∃(?int = int). ?int = ?wt/1 ∧ ?int = ?x) ∧ decode ?x)
        ∧ (∃?y ?wt/2 (?warr/1 = ?y -> ?wt/2).
          ?wu = ?warr/1 ∧ ?wt/2 = ?y ∧ decode ?y))
    in
    decode_scheme ?scheme_final_scheme
  ∃.
    let ?scheme_final_scheme : ?final_term =
    (∃ ?wt/1 ?x ?wu ?final_term (?wt = ?wu -> ?final_term)
      (?warr = ?x -> ?wt/1)
    .
      ∃.
        ?wt = ?warr
        ∧ (∃(?int = int). ?int = ?wt/1 ∧ ?int = ?x)
        ∧ decode ?x
        ∧ (∃?y ?wt/2 (?warr/1 = ?y -> ?wt/2).
          ?wu = ?warr/1 ∧ ?wt/2 = ?y ∧ decode ?y))
    in
    decode_scheme ?scheme_final_scheme
  ∃.
    let ?scheme_final_scheme : ?final_term =
    (∃?wu.
      ∃.
        (∃(?int = int). ?int = ?final_term ∧ ?int = ?wu)
        ∧ decode ?wu
        ∧ (∃?y ?wt/2 (?warr/1 = ?y -> ?wt/2).
          ?wu = ?warr/1 ∧ ?wt/2 = ?y ∧ decode ?y))
    in
    decode_scheme ?scheme_final_scheme
  ∃.
    let ?scheme_final_scheme : ?final_term =
    (∃?wu (?int = int).
      ∃.
        ?int = ?final_term
        ∧ ?int = ?wu
        ∧ decode ?wu
        ∧ (∃?y ?wt/2 (?warr/1 = ?y -> ?wt/2).
          ?wu = ?warr/1 ∧ ?wt/2 = ?y ∧ decode ?y))
    in
    decode_scheme ?scheme_final_scheme
  ∃.
    let ?scheme_final_scheme : ?int =
    (∃?wu (?int' = int). ?int = ?int')
    in
    decode_scheme ?scheme_final_scheme
  ∃.
    let ?scheme_final_scheme : ?int =
    (∃(?int'/1 = int). ?int = ?int'/1)
    in
    decode_scheme ?scheme_final_scheme
  ∃.
    let ?scheme_final_scheme : ?int =
    (∃?y (?int'/2 = int). ?int = ?int'/2)
    in
    decode_scheme ?scheme_final_scheme
  ∃.
    let ?scheme_final_scheme : ?int =
    (∃?wt/2 ?y (?int'/3 = int). ?int = ?int'/3)
    in
    decode_scheme ?scheme_final_scheme
  ∃.
    let ?scheme_final_scheme : ?int =
    (∃?wt/2 ?y (?int'/4 = int). ?int = ?int'/4)
    in
    decode_scheme ?scheme_final_scheme
  
  Error:
      int
    incompatible with
      β -> α
  






## Cyclic types

Unification can sometimes create cyclic types. We decide to reject
these situations with an error. (We could also accept those as they
preserve type-safety, but they have the issue, just like the
OCaml -rectypes option, that they allow to write somewhat-nonsensical
program, and our random term generator will be very good at finding
a lot of those.)

  $ minihell $FLAGS --log-solver selfapp.test
  Input term:
    lambda x. x x
  
  Generated constraint:
    ∃.
      let ?scheme_final_scheme : ?final_term =
      ∃?x ?wt (?warr = ?x -> ?wt).
        ?final_term = ?warr
        ∧ (∃?wu (?wt/1 = ?wu -> ?wt). ?wt/1 = ?x ∧ ?wu = ?x)
        ∧ decode ?x
      in
      decode_scheme ?scheme_final_scheme
  
  Constraint solving log:
  ∃.
    let ?scheme_final_scheme : ?final_term =
    ∃?x ?wt (?warr = ?x -> ?wt).
      ?final_term = ?warr
      ∧ (∃?wu (?wt/1 = ?wu -> ?wt). ?wt/1 = ?x ∧ ?wu = ?x)
      ∧ decode ?x
    in
    decode_scheme ?scheme_final_scheme
  ∃.
    let ?scheme_final_scheme : ?final_term =
    ∃?x ?wt (?warr = ?x -> ?wt).
      ?final_term = ?warr
      ∧ (∃?wu (?wt/1 = ?wu -> ?wt). ?wt/1 = ?x ∧ ?wu = ?x)
      ∧ decode ?x
    in
    decode_scheme ?scheme_final_scheme
  ∃.
    let ?scheme_final_scheme : ?final_term =
    (∃?x.
      ∃?wt (?warr = ?x -> ?wt).
        ?final_term = ?warr
        ∧ (∃?wu (?wt/1 = ?wu -> ?wt). ?wt/1 = ?x ∧ ?wu = ?x)
        ∧ decode ?x)
    in
    decode_scheme ?scheme_final_scheme
  ∃.
    let ?scheme_final_scheme : ?final_term =
    (∃?x ?wt.
      ∃(?warr = ?x -> ?wt).
        ?final_term = ?warr
        ∧ (∃?wu (?wt/1 = ?wu -> ?wt). ?wt/1 = ?x ∧ ?wu = ?x)
        ∧ decode ?x)
    in
    decode_scheme ?scheme_final_scheme
  ∃.
    let ?scheme_final_scheme : ?final_term =
    (∃?x ?wt (?warr = ?x -> ?wt).
      ∃.
        ?final_term = ?warr
        ∧ (∃?wu (?wt/1 = ?wu -> ?wt). ?wt/1 = ?x ∧ ?wu = ?x)
        ∧ decode ?x)
    in
    decode_scheme ?scheme_final_scheme
  ∃.
    let ?scheme_final_scheme : ?final_term =
    (∃?x ?wt (?final_term' = ?x -> ?wt). ?final_term = ?final_term')
    in
    decode_scheme ?scheme_final_scheme
  ∃.
    let ?scheme_final_scheme : ?final_term =
    (∃?x ?wu ?wt (?final_term'/1 = ?x -> ?wt). ?final_term = ?final_term'/1)
    in
    decode_scheme ?scheme_final_scheme
  ∃.
    let ?scheme_final_scheme : ?final_term =
    (∃?wu ?wt (?wt/1 = ?wu -> ?wt) ?x ?wu (?final_term'/2 = ?x -> ?wt).
      ?final_term = ?final_term'/2)
    in
    decode_scheme ?scheme_final_scheme
  ∃.
    let ?scheme_final_scheme : ?final_term =
    (∃?wu ?wt (?wt/1 = ?wu -> ?wt) (?final_term'/3 = ?wt/1 -> ?wt).
      ?final_term = ?final_term'/3)
    in
    decode_scheme ?scheme_final_scheme
  
  Error:
    cycle on constraint variable
    ?wu
  






## Erroneous programs

  $ minihell $FLAGS --log-solver error_poly_def.test
  Fatal error: exception Invalid_argument("Constraint variable 'id' is unbound at this point")
  [2]




## Generator tests

This gives example outputs for my implementation. It is completely
fine if your own implementation produces different (sensible) results.

There are not many programs with size 3, 4 and 5.

  $ minigen --exhaustive --types --size 2 --count 100
  lambda (v : α/1). v
  
  Inferred type : ∀α/1. α/1 -> α/1


  $ minigen --exhaustive --types --size 3 --count 100
  lambda (z/3 : β/4). lambda (y/4 : γ/4). z/3
  
  Inferred type : ∀β/4. ∀γ/4. β/4 -> γ/4 -> β/4
  
  
  
  lambda (z/3 : α/5). lambda (y/4 : δ/4). y/4
  
  Inferred type : ∀α/5. ∀δ/4. α/5 -> δ/4 -> δ/4


  $ minigen --exhaustive --types --size 4 --count 100
  lambda
  (x/10 : β/11). lambda (y/14 : δ/11). lambda (x/15 : γ/11). x/10
  
  Inferred type : ∀δ/11. ∀β/11. ∀γ/11. β/11
  ->
  δ/11 -> γ/11 -> β/11
  
  
  
  lambda
  (x/10 : γ/12). lambda (y/14 : α/12). lambda (x/15 : β/12). y/14
  
  Inferred type : ∀α/12. ∀γ/12. ∀β/12. γ/12
  ->
  α/12 -> β/12 -> α/12
  
  
  
  lambda
  (x/10 : β/13). lambda (y/14 : α/13). lambda (x/15 : δ/12). x/15
  
  Inferred type : ∀α/13. ∀β/13. ∀δ/12. β/13
  ->
  α/13 -> δ/12 -> δ/12
  
  
  
  lambda (x/10 : γ/14). let (z/18 : δ/14) = x/10 in x/10
  
  Inferred type : ∀γ/14. γ/14 -> γ/14
  
  
  
  lambda (x/10 : α/15). let (z/18 : α/15) = x/10 in z/18
  
  Inferred type : ∀α/15. α/15 -> α/15
  
  
  
  lambda (x/10 : β/17). (x/10, x/10)
  
  Inferred type : ∀β/17. β/17 -> {β/17 * β/17}
  
  
  
  lambda
  (x/10 : {γ/19 * δ/19}).
    let ((w/21 : γ/19), (x/22 : δ/19)) = x/10 in x/10
  
  Inferred type : ∀γ/19. ∀δ/19. {γ/19 * δ/19} -> {γ/19 * δ/19}
  
  
  
  lambda
  (x/10 : {α/1a * β/1a}).
    let ((w/21 : α/1a), (x/22 : β/1a)) = x/10 in w/21
  
  Inferred type : ∀α/1a. ∀β/1a. {α/1a * β/1a} -> α/1a
  
  
  
  lambda
  (x/10 : {δ/1a * γ/1a}).
    let ((w/21 : δ/1a), (x/22 : γ/1a)) = x/10 in x/22
  
  Inferred type : ∀δ/1a. ∀γ/1a. {δ/1a * γ/1a} -> γ/1a
  
  
  
  let (y/26 : δ/21 -> δ/21) = lambda (v/29 : δ/21). v/29 in y/26
  
  Inferred type : γ/21 -> γ/21


An example of random sampling output at higher size.

  $ minigen --seed 42 --types --size 6 --count 10
  lambda
  (v/3 : β/27a).
    (lambda (y/34f : γ/27a). v/3, lambda (z/34f : δ/27a). z/34f)
  
  Inferred type : ∀γ/27a. ∀β/27a. ∀δ/27a. β/27a
  ->
  {γ/27a -> β/27a * δ/27a -> δ/27a}
  
  
  
  lambda
  (v/3 : α/298).
    (lambda (y/65 : α/298). lambda (x/8f : δ/297). x/8f) v/3
  
  Inferred type : ∀α/298. ∀δ/297. α/298 -> δ/297 -> δ/297
  
  
  
  lambda
  (v/3 : {α/3cd * δ/3cc}).
    let
    ((z/533 : α/3cd), (u/533 : δ/3cc))
    =
    let ((v/533 : α/3cd), (w/533 : δ/3cc)) = v/3 in v/3
    in u/533
  
  Inferred type : ∀α/3cd. ∀δ/3cc. {α/3cd * δ/3cc} -> δ/3cc
  
  
  
  lambda
  (v/3 : (δ/3df -> γ/3df) -> β/3df).
    lambda (u/22 : γ/3df). v/3 (lambda (w/473 : δ/3df). u/22)
  
  Inferred type : ∀γ/3df. ∀δ/3df. ∀β/3df. ((δ/3df -> γ/3df)
  ->
  β/3df)
  ->
  γ/3df -> β/3df
  
  
  
  let
  (w/2 : β/41a -> {β/41a * β/41a})
  =
  lambda (u/d : β/41a). (u/d, u/d)
  in w/2
  
  Inferred type : α/41a -> {α/41a * α/41a}
  
  
  
  lambda
  (v/3 : {γ/4f1 * δ/4f1}).
    let
    ((x/6c6 : γ/4f1), (y/6c6 : δ/4f1))
    =
    v/3
    in let (z/6c6 : {γ/4f1 * δ/4f1}) = v/3 in z/6c6
  
  Inferred type : ∀γ/4f1. ∀δ/4f1. {γ/4f1 * δ/4f1}
  ->
  {γ/4f1 * δ/4f1}
  
  
  
  lambda
  (v/3 : {α/750 * β/750}).
    let
    (w/11 : {α/750 * β/750})
    =
    let ((y/a19 : α/750), (z/a19 : β/750)) = v/3 in v/3
    in w/11
  
  Inferred type : ∀α/750. ∀β/750. {α/750 * β/750}
  ->
  {α/750 * β/750}
  
  
  
  lambda
  (v/3 : α/83a).
    lambda (u/22 : γ/83a). (lambda (y/b57 : β/83a). v/3, u/22)
  
  Inferred type : ∀γ/83a. ∀α/83a. ∀β/83a. α/83a
  ->
  γ/83a -> {β/83a -> α/83a * γ/83a}
  
  
  
  let
  (w/2 : β/b57 -> α/b57 -> α/b57)
  =
  lambda (u/d : β/b57). lambda (v/58 : α/b57). v/58
  in lambda (u/75 : δ/b56). u/75
  
  Inferred type : ∀δ/b56. δ/b56 -> δ/b56


  
  lambda
  (v/3 : {γ/bfe * β/bfe}).
  (v/3 : {γ/bff * β/bff}).
    let
    ((v/109c : γ/bfe), (w/109c : β/bfe))
    ((v/109c : γ/bff), (w/109c : β/bff))
     =
    v/3
    in let (x/109d : β/bfe) = w/109c in x/109d
    in let (x/109d : β/bff) = w/109c in x/109d
  
  Inferred type : ∀γ/bff. ∀β/bff. {γ/bff * β/bff} -> β/bff
