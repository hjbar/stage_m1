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
    lambda (x : int). x
  



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
    lambda (x : int). x
  



## An erroneous program

  $ minihell $FLAGS error.test
  Input term:
    lambda x. (x : int) lambda y. y
  
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

  $ minigen --search exhaustive --types --size 2 --count 100
  lambda (z/3 : α/1). z/3
  
  Inferred type : α/1 -> α/1

  $ minigen --search exhaustive --types --size 3 --count 100
  lambda (x/11 : β/4). lambda (y/15 : γ/4). x/11
  
  Inferred type : β/4 -> γ/4 -> β/4
  
  
  
  lambda (x/11 : α/5). lambda (y/15 : δ/4). y/15
  
  Inferred type : α/5 -> δ/4 -> δ/4

  $ minigen --search exhaustive --types --size 4 --count 100
  lambda
  (v/4e : β/11). lambda (u/65 : δ/11). lambda (z/6a : γ/11). v/4e
  
  Inferred type : β/11 -> δ/11 -> γ/11 -> β/11
  
  
  
  lambda
  (v/4e : γ/12). lambda (u/65 : α/12). lambda (z/6a : β/12). u/65
  
  Inferred type : γ/12 -> α/12 -> β/12 -> α/12
  
  
  
  lambda
  (v/4e : β/13). lambda (u/65 : α/13). lambda (z/6a : δ/12). z/6a
  
  Inferred type : β/13 -> α/13 -> δ/12 -> δ/12
  
  
  
  lambda (v/4e : γ/14). let (z/7e : γ/14) = v/4e in v/4e
  
  Inferred type : γ/14 -> γ/14
  
  
  
  lambda (v/4e : δ/14). let (z/7e : δ/14) = v/4e in z/7e
  
  Inferred type : δ/14 -> δ/14
  
  
  
  lambda (v/4e : α/17). (v/4e, v/4e)
  
  Inferred type : α/17 -> {α/17 * α/17}
  
  
  
  lambda
  (v/4e : {β/19 * γ/19}).
    let ((y/b6 : β/19), (z/b5 : γ/19)) = v/4e in v/4e
  
  Inferred type : {β/19 * γ/19} -> {β/19 * γ/19}
  
  
  
  lambda
  (v/4e : {α/1a * δ/19}).
    let ((y/b6 : α/1a), (z/b5 : δ/19)) = v/4e in z/b5
  
  Inferred type : {α/1a * δ/19} -> δ/19
  
  
  
  lambda
  (v/4e : {β/1a * γ/1a}).
    let ((y/b6 : β/1a), (z/b5 : γ/1a)) = v/4e in y/b6
  
  Inferred type : {β/1a * γ/1a} -> β/1a
  
  
  
  let (u/cb : β/21 -> β/21) = lambda (v/db : β/21). v/db in u/cb
  
  Inferred type : β/21 -> β/21

An example of random sampling output at higher size.

  $ minigen --seed 42 --types --size 6 --count 10
  lambda
  (v/5 : δ/16c).
    lambda
    (u/6 : {β/16c * γ/16c}).
      lambda
      (w/71 : α/16c). let ((z/284 : β/16c), (u/284 : γ/16c)) = u/6 in w/71
  
  Inferred type : δ/16c -> {β/16c * γ/16c} -> α/16c -> α/16c
  
  
  
  lambda
  (v/5 : β/1de).
    (lambda (x/336 : α/1de). lambda (y/336 : δ/1dd). y/336, v/5)
  
  Inferred type : β/1de -> {α/1de -> δ/1dd -> δ/1dd * β/1de}
  
  
  
  lambda
  (v/5 : β/271). let (v/18 : {β/271 * β/271}) = (v/5, v/5) in v/18
  
  Inferred type : β/271 -> {β/271 * β/271}
  
  
  
  lambda
  (v/5 : {{δ/279 * α/27a} * β/27a}).
    let
    ((v/43e : {δ/279 * α/27a}), (w/43e : β/27a))
    =
    v/5
    in let ((x/440 : δ/279), (y/440 : α/27a)) = v/43e in x/440
  
  Inferred type : {{δ/279 * α/27a} * β/27a} -> δ/279
  
  
  
  lambda (v/5 : β/2a5). ((v/5, v/5), v/5)
  
  Inferred type : β/2a5 -> {{β/2a5 * β/2a5} * β/2a5}
  
  
  
  let
  (x/1 : α/2ba -> γ/2b9 -> δ/2b9 -> γ/2b9)
  =
  lambda
  (x/2f : α/2ba). lambda (y/2f : γ/2b9). lambda (y/15d : δ/2b9). y/2f
  in x/1
  
  Inferred type : α/2ba -> γ/2b9 -> δ/2b9 -> γ/2b9
  
  
  
  (lambda (z/f : α/403 -> δ/402 -> δ/402). z/f)
    (lambda (v/26 : α/403). lambda (y/1bc : δ/402). y/1bc)
  
  Inferred type : α/403 -> δ/402 -> δ/402
  
  
  
  lambda
  (v/5 : β/4f4). let (v/18 : {β/4f4 * β/4f4}) = (v/5, v/5) in v/5
  
  Inferred type : β/4f4 -> β/4f4
  
  
  
  lambda
  (v/5 : α/523).
    lambda
    (u/6 : δ/523).
      lambda
      (w/71 : {β/523 * γ/523}).
        let ((z/88d : β/523), (u/88d : γ/523)) = w/71 in v/5
  
  Inferred type : α/523 -> δ/523 -> {β/523 * γ/523} -> α/523
  
  
  
  lambda
  (v/5 : {α/59a * β/59a}).
    let
    (v/18 : {α/59a * β/59a})
    =
    let ((z/93d : α/59a), (u/93d : β/59a)) = v/5 in v/5
    in v/18
  
  Inferred type : {α/59a * β/59a} -> {α/59a * β/59a}
