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

  $ FLAGS="--show-source --show-constraint"

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
    ∃?final_type.
      (∃?x ?wt (?warr = ?x -> ?wt). ?final_type = ?warr ∧ decode ?x ∧ ?wt = ?x)
      ∧ decode ?final_type

  Inferred type:
    α -> α

  Elaborated term:
    lambda (x : α). x




`id_int` is the monomorphic identity on the type `int`. Note
that we have not implemented support for a built-in `int`
type, this is just an abstract/rigid type variable: `Constr
(Var ...)` at type `STLC.ty`.

  $ minihell $FLAGS id_int.test
  Input term:
    lambda x. (x : int)

  Generated constraint:
    ∃?final_type.
      (∃?x ?wt (?warr = ?x -> ?wt).
        ?final_type = ?warr
        ∧ decode ?x
        ∧ (∃(?int = int). ?int = ?x ∧ ?int = ?wt))
      ∧ decode ?final_type

  Inferred type:
    int -> int

  Elaborated term:
    lambda (x : int). (x : int)




## Logging the constraint-solving process

You can ask `minihell` to show how the constraint evolves as
the solver progresses and accumulates more information on
the inference variables.

  $ minihell $FLAGS --log-solver id_int.test
  Input term:
    lambda x. (x : int)

  Generated constraint:
    ∃?final_type.
      (∃?x ?wt (?warr = ?x -> ?wt).
        ?final_type = ?warr
        ∧ decode ?x
        ∧ (∃(?int = int). ?int = ?x ∧ ?int = ?wt))
      ∧ decode ?final_type

  Constraint solving log:
    ∃?final_type.
      decode ?final_type
      ∧ (∃?x ?wt (?warr = ?x -> ?wt).
        (∃(?int = int). ?int = ?wt ∧ ?int = ?x)
        ∧ decode ?x
        ∧ ?final_type = ?warr)
    ∃?final_type.
      decode ?final_type
      ∧ (∃?x ?wt (?warr = ?x -> ?wt).
        (∃(?int = int). ?int = ?wt ∧ ?int = ?x)
        ∧ decode ?x
        ∧ ?final_type = ?warr)
    ∃?x ?final_type.
      decode ?final_type
      ∧ (∃?wt (?warr = ?x -> ?wt).
        (∃(?int = int). ?int = ?wt ∧ ?int = ?x)
        ∧ decode ?x
        ∧ ?final_type = ?warr)
    ∃?x ?wt ?final_type.
      decode ?final_type
      ∧ (∃(?warr = ?x -> ?wt).
        (∃(?int = int). ?int = ?wt ∧ ?int = ?x)
        ∧ decode ?x
        ∧ ?final_type = ?warr)
    ∃?x ?wt (?warr = ?x -> ?wt) ?final_type.
      decode ?final_type
      ∧ (∃(?int = int). ?int = ?wt ∧ ?int = ?x)
      ∧ decode ?x
      ∧ ?final_type = ?warr
    ∃?x ?wt (?final_type = ?x -> ?wt).
      decode ?final_type ∧ (∃(?int = int). ?int = ?wt ∧ ?int = ?x) ∧ decode ?x
    ∃?x ?wt (?int = int) (?final_type = ?x -> ?wt).
      decode ?final_type ∧ ?int = ?wt ∧ ?int = ?x ∧ decode ?x
    ∃?wt (?int = int) (?final_type = ?int -> ?wt).
      decode ?final_type ∧ ?int = ?wt ∧ decode ?int
    ∃(?int = int) (?final_type = ?int -> ?int).
      decode ?final_type ∧ decode ?int

  Inferred type:
    int -> int

  Elaborated term:
    lambda (x : int). (x : int)




## An erroneous program

  $ minihell $FLAGS error.test
  Input term:
    (lambda x. (x : int)) (lambda y. y)

  Generated constraint:
    ∃?final_type.
      (∃?wu (?wt = ?wu -> ?final_type).
        (∃?x ?wt/1 (?warr = ?x -> ?wt/1).
          ?wt = ?warr ∧ decode ?x ∧ (∃(?int = int). ?int = ?x ∧ ?int = ?wt/1))
        ∧ (∃?y ?wt/2 (?warr/1 = ?y -> ?wt/2).
          ?wu = ?warr/1 ∧ decode ?y ∧ ?wt/2 = ?y))
      ∧ decode ?final_type

  Error:
      int
    incompatible with
      β -> α




## Examples with products

  $ minihell $FLAGS curry.test
  Input term:
    lambda f. lambda x. lambda y. f (x, y)

  Generated constraint:
    ∃?final_type.
      (∃?f ?wt (?warr = ?f -> ?wt).
        ?final_type = ?warr
        ∧ decode ?f
        ∧ (∃?x ?wt/1 (?warr/1 = ?x -> ?wt/1).
          ?wt = ?warr/1
          ∧ decode ?x
          ∧ (∃?y ?wt/2 (?warr/2 = ?y -> ?wt/2).
            ?wt/1 = ?warr/2
            ∧ decode ?y
            ∧ (∃?wu (?wt/3 = ?wu -> ?wt/2).
              ?wt/3 = ?f
              ∧ (∃?w1.
                ?w1 = ?x
                ∧ (∃?w2. ?w2 = ?y ∧ (∃(?wprod = {?w1 * ?w2}). ?wu = ?wprod)))))))
      ∧ decode ?final_type

  Inferred type:
    ({γ * β} -> α) -> γ -> β -> α

  Elaborated term:
    lambda (f : {γ * β} -> α). lambda (x : γ). lambda (y : β). f (x, y)




  $ minihell $FLAGS uncurry.test
  Input term:
    lambda f. lambda p. let (x, y) = p in f x y

  Generated constraint:
    ∃?final_type.
      (∃?f ?wt (?warr = ?f -> ?wt).
        ?final_type = ?warr
        ∧ decode ?f
        ∧ (∃?p ?wt/1 (?warr/1 = ?p -> ?wt/1).
          ?wt = ?warr/1
          ∧ decode ?p
          ∧ (∃?x ?y (?wt/2 = {?x * ?y}).
            decode ?x
            ∧ decode ?y
            ∧ ?wt/2 = ?p
            ∧ (∃?wu (?wt/3 = ?wu -> ?wt/1).
              (∃?wu/1 (?wt/4 = ?wu/1 -> ?wt/3). ?wt/4 = ?f ∧ ?wu/1 = ?x)
              ∧ ?wu = ?y))))
      ∧ decode ?final_type

  Inferred type:
    (β -> γ -> α) -> {β * γ} -> α

  Elaborated term:
    lambda
    (f : β -> γ -> α).
      lambda (p : {β * γ}). let ((x : β), (y : γ)) = p in f x y



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
    ∃?final_type.
      (∃?x ?wt (?warr = ?x -> ?wt).
        ?final_type = ?warr
        ∧ decode ?x
        ∧ (∃?wu (?wt/1 = ?wu -> ?wt). ?wt/1 = ?x ∧ ?wu = ?x))
      ∧ decode ?final_type

  Constraint solving log:
    ∃?final_type.
      decode ?final_type
      ∧ (∃?x ?wt (?warr = ?x -> ?wt).
        (∃?wu (?wt/1 = ?wu -> ?wt). ?wu = ?x ∧ ?wt/1 = ?x)
        ∧ decode ?x
        ∧ ?final_type = ?warr)
    ∃?final_type.
      decode ?final_type
      ∧ (∃?x ?wt (?warr = ?x -> ?wt).
        (∃?wu (?wt/1 = ?wu -> ?wt). ?wu = ?x ∧ ?wt/1 = ?x)
        ∧ decode ?x
        ∧ ?final_type = ?warr)
    ∃?x ?final_type.
      decode ?final_type
      ∧ (∃?wt (?warr = ?x -> ?wt).
        (∃?wu (?wt/1 = ?wu -> ?wt). ?wu = ?x ∧ ?wt/1 = ?x)
        ∧ decode ?x
        ∧ ?final_type = ?warr)
    ∃?x ?wt ?final_type.
      decode ?final_type
      ∧ (∃(?warr = ?x -> ?wt).
        (∃?wu (?wt/1 = ?wu -> ?wt). ?wu = ?x ∧ ?wt/1 = ?x)
        ∧ decode ?x
        ∧ ?final_type = ?warr)
    ∃?x ?wt (?warr = ?x -> ?wt) ?final_type.
      decode ?final_type
      ∧ (∃?wu (?wt/1 = ?wu -> ?wt). ?wu = ?x ∧ ?wt/1 = ?x)
      ∧ decode ?x
      ∧ ?final_type = ?warr
    ∃?x ?wt (?final_type = ?x -> ?wt).
      decode ?final_type
      ∧ (∃?wu (?wt/1 = ?wu -> ?wt). ?wu = ?x ∧ ?wt/1 = ?x)
      ∧ decode ?x
    ∃?x ?wu ?wt (?final_type = ?x -> ?wt).
      decode ?final_type
      ∧ (∃(?wt/1 = ?wu -> ?wt). ?wu = ?x ∧ ?wt/1 = ?x)
      ∧ decode ?x
    ∃?wu ?wt (?wt/1 = ?wu -> ?wt) ?x ?wu ?wt (?final_type = ?x -> ?wt).
      decode ?final_type ∧ ?wu = ?x ∧ ?wt/1 = ?x ∧ decode ?x
    ∃?wu ?wt (?wt/1 = ?wu -> ?wt) ?wt (?final_type = ?wt/1 -> ?wt).
      decode ?final_type ∧ ⊥ ∧ decode ?wt/1

  Error:
    cycle on constraint variable
    ?wu



## Generator tests

This gives example outputs for my implementation. It is completely
fine if your own implementation produces different (sensible) results.

There are not many programs with size 3, 4 and 5.

  $ minigen --exhaustive --size 2 --count 100
  lambda (v : α/1). v

  $ minigen --exhaustive --size 3 --count 100
  lambda (z/3 : γ/4). lambda (y/4 : β/4). z/3

  lambda (z/3 : α/5). lambda (y/4 : δ/4). y/4

  $ minigen --exhaustive --size 4 --count 100
  lambda
  (x/10 : δ/11). lambda (y/14 : γ/11). lambda (x/15 : β/11). x/10

  lambda
  (x/10 : γ/12). lambda (y/14 : β/12). lambda (x/15 : α/12). y/14

  lambda
  (x/10 : β/13). lambda (y/14 : α/13). lambda (x/15 : δ/12). x/15

  lambda (x/10 : γ/14). let (z/18 : γ/14) = x/10 in x/10

  lambda (x/10 : δ/14). let (z/18 : δ/14) = x/10 in z/18

  lambda (x/10 : α/17). (x/10, x/10)

  lambda
  (x/10 : {γ/19 * β/19}).
    let ((w/21 : γ/19), (x/22 : β/19)) = x/10 in x/10

  lambda
  (x/10 : {α/1a * δ/19}).
    let ((w/21 : α/1a), (x/22 : δ/19)) = x/10 in w/21

  lambda
  (x/10 : {γ/1a * β/1a}).
    let ((w/21 : γ/1a), (x/22 : β/1a)) = x/10 in x/22

  let (y/26 : β/21 -> β/21) = lambda (v/29 : β/21). v/29 in y/26

An example of random sampling output at higher size.

  $ minigen --seed 42 --size 6 --count 10
  lambda
  (v/3 : δ/27a).
    (lambda (y/34f : γ/27a). v/3, lambda (z/34f : β/27a). z/34f)

  lambda
  (v/3 : α/298).
    (lambda (y/65 : α/298). lambda (x/8f : δ/297). x/8f) v/3

  lambda
  (v/3 : {β/3cc * α/3cc}).
    let
    ((z/533 : β/3cc), (u/533 : α/3cc))
    =
    let ((v/533 : β/3cc), (w/533 : α/3cc)) = v/3 in v/3
    in u/533

  lambda
  (v/3 : (γ/3de -> δ/3de) -> α/3df).
    lambda (u/22 : δ/3de). v/3 (lambda (w/473 : γ/3de). u/22)

  let
  (w/2 : β/419 -> {β/419 * β/419})
  =
  lambda (u/d : β/419). (u/d, u/d)
  in w/2

  lambda
  (v/3 : {γ/4f0 * δ/4f0}).
    let
    ((x/6c6 : γ/4f0), (y/6c6 : δ/4f0))
    =
    v/3
    in let (z/6c6 : {γ/4f0 * δ/4f0}) = v/3 in z/6c6

  lambda
  (v/3 : {α/74f * β/74f}).
    let
    (w/11 : {α/74f * β/74f})
    =
    let ((y/a19 : α/74f), (z/a19 : β/74f)) = v/3 in v/3
    in w/11

  lambda
  (v/3 : γ/839).
    lambda (u/22 : β/839). (lambda (y/b57 : α/839). v/3, u/22)

  let
  (w/2 : β/b56 -> α/b56 -> α/b56)
  =
  lambda (u/d : β/b56). lambda (v/58 : α/b56). v/58
  in lambda (u/75 : δ/b55). u/75

  lambda
  (v/3 : {γ/bfe * β/bfe}).
    let
    ((v/109c : γ/bfe), (w/109c : β/bfe))
    =
    v/3
    in let (x/109d : β/bfe) = w/109c in x/109d

## Polymorhpism

  $ minihell $FLAGS poly1.test

  Input term:
    let
    id
    =
    lambda x. x
    in lambda a. lambda b. let l = id a in let r = id b in (l, r)

  Inferred type:
    α -> β -> {α * β}

  Elaborated term:
    let
    (id : α -> α)
    =
    lambda (x : α). x
    in
      lambda (a : α).
        lambda (b : β). let (l : α) = id a in let (r : β) = id b in (l, r)

  $ minihell $FLAGS id_poly2.test

## Log-Solver

  $ minihell --show-constraint --show-type --show-typed-term --log-solver log_solver.test

  Generated constraint:
    ∃?final_type.
      (∃.
        let ?scheme_s : ?f =
        ∃?x ?wt/2 (?warr/1 = ?x -> ?wt/2). ?f = ?warr/1 ∧ ?wt/2 = ?x ∧ decode ?x
        in
        (∃?y ?wt (?warr = ?y -> ?wt).
          ?final_type = ?warr
          ∧ (∃?wu (?wt/1 = ?wu -> ?wt). ?scheme_s ≤ ?wt/1 ∧ ?wu = ?y)
          ∧ decode ?y))
      ∧ decode_scheme ?scheme_s
      ∧ decode ?final_type

  Constraint solving log:
    ∃?final_type.
      decode ?final_type
      ∧ decode_scheme ?scheme_s
      ∧ (∃.
        let ?scheme_s : ?f =
        ∃?x ?wt/2 (?warr/1 = ?x -> ?wt/2). decode ?x ∧ ?wt/2 = ?x ∧ ?f = ?warr/1
        in
        (∃?y ?wt (?warr = ?y -> ?wt).
          decode ?y
          ∧ (∃?wu (?wt/1 = ?wu -> ?wt). ?wu = ?y ∧ ?scheme_s ≤ ?wt/1)
          ∧ ?final_type = ?warr))
    ∃?final_type.
      decode ?final_type
      ∧ decode_scheme ?scheme_s
      ∧ (∃.
        let ?scheme_s : ?f =
        ∃?x ?wt/2 (?warr/1 = ?x -> ?wt/2). decode ?x ∧ ?wt/2 = ?x ∧ ?f = ?warr/1
        in
        (∃?y ?wt (?warr = ?y -> ?wt).
          decode ?y
          ∧ (∃?wu (?wt/1 = ?wu -> ?wt). ?wu = ?y ∧ ?scheme_s ≤ ?wt/1)
          ∧ ?final_type = ?warr))
    ∃?final_type.
      decode ?final_type
      ∧ decode_scheme ?scheme_s
      ∧ (∃.
        let ?scheme_s : ?f =
        ∃?x ?wt/2 (?warr/1 = ?x -> ?wt/2). decode ?x ∧ ?wt/2 = ?x ∧ ?f = ?warr/1
        in
        (∃?y ?wt (?warr = ?y -> ?wt).
          decode ?y
          ∧ (∃?wu (?wt/1 = ?wu -> ?wt). ?wu = ?y ∧ ?scheme_s ≤ ?wt/1)
          ∧ ?final_type = ?warr))
    ∃?final_type.
      decode ?final_type
      ∧ decode_scheme ?scheme_s
      ∧ (∃.
        let ?scheme_s : ?f =
        ∃?x ?wt/2 (?warr/1 = ?x -> ?wt/2). decode ?x ∧ ?wt/2 = ?x ∧ ?f = ?warr/1
        in
        (∃?y ?wt (?warr = ?y -> ?wt).
          decode ?y
          ∧ (∃?wu (?wt/1 = ?wu -> ?wt). ?wu = ?y ∧ ?scheme_s ≤ ?wt/1)
          ∧ ?final_type = ?warr))
    ∃?final_type.
      decode ?final_type
      ∧ decode_scheme ?scheme_s
      ∧ (∃?x.
        let ?scheme_s : ?f =
        ∃?wt/2 (?warr/1 = ?x -> ?wt/2). decode ?x ∧ ?wt/2 = ?x ∧ ?f = ?warr/1
        in
        (∃?y ?wt (?warr = ?y -> ?wt).
          decode ?y
          ∧ (∃?wu (?wt/1 = ?wu -> ?wt). ?wu = ?y ∧ ?scheme_s ≤ ?wt/1)
          ∧ ?final_type = ?warr))
    ∃?final_type.
      decode ?final_type
      ∧ decode_scheme ?scheme_s
      ∧ (∃?wt/2 ?x.
        let ?scheme_s : ?f =
        ∃(?warr/1 = ?x -> ?wt/2). decode ?x ∧ ⊥ ∧ ?f = ?warr/1
        in
        (∃?y ?wt (?warr = ?y -> ?wt).
          decode ?y
          ∧ (∃?wu (?wt/1 = ?wu -> ?wt). ?wu = ?y ∧ ?scheme_s ≤ ?wt/1)
          ∧ ?final_type = ?warr))
    ∃?final_type.
      decode ?final_type
      ∧ decode_scheme ?scheme_s
      ∧ (∃?x.
        let ?scheme_s : ?f =
        ∃. decode ?x ∧ ⊥ ∧ ⊥
        in
        (∃?y ?wt (?warr = ?y -> ?wt).
          decode ?y
          ∧ (∃?wu (?wt/1 = ?wu -> ?wt). ?wu = ?y ∧ ?scheme_s ≤ ?wt/1)
          ∧ ?final_type = ?warr))
    ∃?final_type.
      decode ?final_type
      ∧ decode_scheme ?scheme_s
      ∧ (∃?x.
        let ?scheme_s : ?f =
        ∃. decode ?x ∧ ⊥
        in
        (∃?y ?wt (?warr = ?y -> ?wt).
          decode ?y
          ∧ (∃?wu (?wt/1 = ?wu -> ?wt). ?wu = ?y ∧ ?scheme_s ≤ ?wt/1)
          ∧ ?final_type = ?warr))
    ∃?final_type.
      decode ?final_type
      ∧ decode_scheme ?scheme_s
      ∧ (∃?wt/2.
        let ?scheme_s : ?f =
        ∃. decode ?wt/2
        in
        (∃?y ?wt (?warr = ?y -> ?wt).
          decode ?y
          ∧ (∃?wu (?wt/1 = ?wu -> ?wt). ?wu = ?y ∧ ?scheme_s ≤ ?wt/1)
          ∧ ?final_type = ?warr))
    ∃?final_type.
      decode ?final_type
      ∧ decode_scheme ?scheme_s
      ∧ (∃?wt/2.
        let ?scheme_s : ?f =
        ∃. decode ?wt/2
        in
        (∃?y ?wt (?warr = ?y -> ?wt).
          decode ?y
          ∧ (∃?wu (?wt/1 = ?wu -> ?wt). ?wu = ?y ∧ ?scheme_s ≤ ?wt/1)
          ∧ ?final_type = ?warr))
    ∃?y ?final_type.
      decode ?final_type
      ∧ decode_scheme ?scheme_s
      ∧ (∃?wt/2.
        let ?scheme_s : ?f =
        ∃. decode ?wt/2
        in
        (∃?wt (?warr = ?y -> ?wt).
          decode ?y
          ∧ (∃?wu (?wt/1 = ?wu -> ?wt). ?wu = ?y ∧ ?scheme_s ≤ ?wt/1)
          ∧ ?final_type = ?warr))
    ∃?y ?wt ?final_type.
      decode ?final_type
      ∧ decode_scheme ?scheme_s
      ∧ (∃?wt/2.
        let ?scheme_s : ?f =
        ∃. decode ?wt/2
        in
        (∃(?warr = ?y -> ?wt).
          decode ?y
          ∧ (∃?wu (?wt/1 = ?wu -> ?wt). ?wu = ?y ∧ ?scheme_s ≤ ?wt/1)
          ∧ ?final_type = ?warr))
    ∃?y ?wt ?final_type.
      decode ?final_type
      ∧ decode_scheme ?scheme_s
      ∧ (∃?wt/2.
        let ?scheme_s : ?f =
        ∃. decode ?wt/2
        in
        (∃.
          decode ?y
          ∧ (∃?wu (?wt/1 = ?wu -> ?wt). ?wu = ?y ∧ ?scheme_s ≤ ?wt/1)
          ∧ ⊥))
    ∃?y ?wt (?final_type = ?y -> ?wt).
      decode ?final_type
      ∧ decode_scheme ?scheme_s
      ∧ (∃?wt/2.
        let ?scheme_s : ?f =
        ∃. decode ?wt/2
        in
        (∃.
          decode ?y ∧ (∃?wu (?wt/1 = ?wu -> ?wt). ?wu = ?y ∧ ?scheme_s ≤ ?wt/1)))
    ∃?y ?wu ?wt (?final_type = ?y -> ?wt).
      decode ?final_type
      ∧ decode_scheme ?scheme_s
      ∧ (∃?wt/2.
        let ?scheme_s : ?f =
        ∃. decode ?wt/2
        in
        (∃. decode ?y ∧ (∃(?wt/1 = ?wu -> ?wt). ⊥ ∧ ?scheme_s ≤ ?wt/1)))
    ∃?wu ?wt (?wt/1 = ?wu -> ?wt) ?y ?wt (?final_type = ?y -> ?wt).
      decode ?final_type
      ∧ decode_scheme ?scheme_s
      ∧ (∃?wt/2.
        let ?scheme_s : ?f =
        ∃. decode ?wt/2
        in
        (∃. decode ?y ∧ ⊥ ∧ ?scheme_s ≤ ?wt/1))
    ∃?y (?final_type = ?y -> ?wt/2).
      decode ?final_type
      ∧ decode_scheme ?scheme_s
      ∧ (∃?wt/2 (?f = ?wt/2 -> ?wt/2).
        let ?scheme_s : ?f =
        ∃. decode ?wt/2
        in
        (∃. decode ?y ∧ ⊥ ∧ ?scheme_s ≤ ?f))
    ∃(?final_type = ?wt/2 -> ?wt/2).
      decode ?final_type
      ∧ decode_scheme ?scheme_s
      ∧ (∃?wt/2 (?f = ?wt/2 -> ?wt/2).
        let ?scheme_s : ?f =
        ∃. decode ?wt/2
        in
        (∃. decode ?wt/2 ∧ ?scheme_s ≤ ?f))

  Inferred type:
    α -> α

  Elaborated term:
    let (f : α -> α) = lambda (x : α). x in lambda (y : α). f y
