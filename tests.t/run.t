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
      (∃?x ?wt (?warr = ?x -> ?wt). ?final_type = ?warr ∧ ?wt = ?x ∧ decode ?x)
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
        ∧ (∃(?int = int). ?int = ?wt ∧ ?int = ?x)
        ∧ decode ?x)
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
        ∧ (∃(?int = int). ?int = ?wt ∧ ?int = ?x)
        ∧ decode ?x)
      ∧ decode ?final_type
  
  Constraint solving log:
  -- hole {}
    (
      ∃?final_type.
        (∃?x ?wt (?warr = ?x -> ?wt).
          ?final_type = ?warr
          ∧ (∃(?int = int). ?int = ?wt ∧ ?int = ?x)
          ∧ decode ?x)
        ∧ decode ?final_type
    )
  -> hole {final_type }
    (
      (∃?x ?wt (?warr = ?x -> ?wt).
        ?final_type = ?warr
        ∧ (∃(?int = int). ?int = ?wt ∧ ?int = ?x)
        ∧ decode ?x)
      ∧ decode ?final_type
    )
  -> ∃?final_type.
      hole
      {
        final_type
        x
      }
      (
        ∃?wt (?warr = ?x -> ?wt).
          ?final_type = ?warr
          ∧ (∃(?int = int). ?int = ?wt ∧ ?int = ?x)
          ∧ decode ?x
      )
    ∧ decode ?final_type
  -> ∃?x.
      ∃?final_type.
        hole
        {
          final_type
          wt
          x
        }
        (
          ∃(?warr = ?x -> ?wt).
            ?final_type = ?warr
            ∧ (∃(?int = int). ?int = ?wt ∧ ?int = ?x)
            ∧ decode ?x
        )
      ∧ decode ?final_type
  -> ∃?wt.
      ∃?x.
        ∃?final_type.
          hole
          {
            final_type
            warr = x -> wt
            wt
            x
          }
          (
            ?final_type = ?warr
            ∧ (∃(?int = int). ?int = ?wt ∧ ?int = ?x)
            ∧ decode ?x
          )
        ∧ decode ?final_type
  -> ∃?warr.
      ∃?wt.
        ∃?x.
          ∃?final_type.
            hole
            {
              final_type = x -> wt
              warr |--> final_type
              wt
              x
            }
            (?final_type = ?warr)
          ∧ decode ?final_type
    ∧ decode ?x
    ∧ ∃(?int = int). ?int = ?wt ∧ ?int = ?x
  -> ⊤
    ∧ ∃?warr.
      ∃?wt.
        ∃?x.
          ∃?final_type.
            hole
            {
              final_type = x -> wt
              int = int
              warr |--> final_type
              wt
              x
            }
            (?int = ?wt ∧ ?int = ?x)
          ∧ decode ?final_type
    ∧ decode ?x
  -> ∃?int.
      ⊤
      ∧ ∃?warr.
        ∃?wt.
          ∃?x.
            ∃?final_type.
              hole
              {
                final_type = x -> int
                int = int
                warr |--> final_type
                wt |--> int
                x
              }
              (?int = ?wt)
            ∧ decode ?final_type
      ∧ decode ?x
    ∧ ?int = ?x
  -> ⊤
    ∧ ∃?int.
      ⊤
      ∧ ∃?warr.
        ∃?wt.
          ∃?x.
            ∃?final_type.
              hole
              {
                final_type = int -> int
                int = int
                warr |--> final_type
                wt |--> int
                x |--> int
              }
              (?int = ?x)
            ∧ decode ?final_type
      ∧ decode ?x
  
  Inferred type:
    int -> int
  
  Elaborated term:
    lambda (x : int). (x : int)
  






## An erroneous program

  $ minihell $FLAGS error_incompatible_types.test
  Input term:
    lambda x. (x : int) lambda y. y
  
  Generated constraint:
    ∃?final_type.
      (∃?wu (?wt = ?wu -> ?final_type).
        (∃?x ?wt/1 (?warr = ?x -> ?wt/1).
          ?wt = ?warr ∧ (∃(?int = int). ?int = ?wt/1 ∧ ?int = ?x) ∧ decode ?x)
        ∧ (∃?y ?wt/2 (?warr/1 = ?y -> ?wt/2).
          ?wu = ?warr/1 ∧ ?wt/2 = ?y ∧ decode ?y))
      ∧ decode ?final_type
  
  File "error_incompatible_types.test", line 1, characters 23-34:
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
        ∧ decode ?f)
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
        ∧ decode ?f)
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
        ∧ (∃?wu (?wt/1 = ?wu -> ?wt). ?wt/1 = ?x ∧ ?wu = ?x)
        ∧ decode ?x)
      ∧ decode ?final_type
  
  Constraint solving log:
  -- hole {}
    (
      ∃?final_type.
        (∃?x ?wt (?warr = ?x -> ?wt).
          ?final_type = ?warr
          ∧ (∃?wu (?wt/1 = ?wu -> ?wt). ?wt/1 = ?x ∧ ?wu = ?x)
          ∧ decode ?x)
        ∧ decode ?final_type
    )
  -> hole {final_type }
    (
      (∃?x ?wt (?warr = ?x -> ?wt).
        ?final_type = ?warr
        ∧ (∃?wu (?wt/1 = ?wu -> ?wt). ?wt/1 = ?x ∧ ?wu = ?x)
        ∧ decode ?x)
      ∧ decode ?final_type
    )
  -> ∃?final_type.
      hole
      {
        final_type
        x
      }
      (
        ∃?wt (?warr = ?x -> ?wt).
          ?final_type = ?warr
          ∧ (∃?wu (?wt/1 = ?wu -> ?wt). ?wt/1 = ?x ∧ ?wu = ?x)
          ∧ decode ?x
      )
    ∧ decode ?final_type
  -> ∃?x.
      ∃?final_type.
        hole
        {
          final_type
          wt
          x
        }
        (
          ∃(?warr = ?x -> ?wt).
            ?final_type = ?warr
            ∧ (∃?wu (?wt/1 = ?wu -> ?wt). ?wt/1 = ?x ∧ ?wu = ?x)
            ∧ decode ?x
        )
      ∧ decode ?final_type
  -> ∃?wt.
      ∃?x.
        ∃?final_type.
          hole
          {
            final_type
            warr = x -> wt
            wt
            x
          }
          (
            ?final_type = ?warr
            ∧ (∃?wu (?wt/1 = ?wu -> ?wt). ?wt/1 = ?x ∧ ?wu = ?x)
            ∧ decode ?x
          )
        ∧ decode ?final_type
  -> ∃?warr.
      ∃?wt.
        ∃?x.
          ∃?final_type.
            hole
            {
              final_type = x -> wt
              warr |--> final_type
              wt
              x
            }
            (?final_type = ?warr)
          ∧ decode ?final_type
    ∧ decode ?x
    ∧ ∃?wu (?wt/1 = ?wu -> ?wt). ?wt/1 = ?x ∧ ?wu = ?x
  -> ⊤
    ∧ ∃?warr.
      ∃?wt.
        ∃?x.
          ∃?final_type.
            hole
            {
              final_type = x -> wt
              warr |--> final_type
              wt
              wu
              x
            }
            (∃(?wt/1 = ?wu -> ?wt). ?wt/1 = ?x ∧ ?wu = ?x)
          ∧ decode ?final_type
    ∧ decode ?x
  -> ∃?wu.
      ⊤
      ∧ ∃?warr.
        ∃?wt.
          ∃?x.
            ∃?final_type.
              hole
              {
                final_type = x -> wt
                warr |--> final_type
                wt
                wu
                x
                wt/1 = wu -> wt
              }
              (?wt/1 = ?x ∧ ?wu = ?x)
            ∧ decode ?final_type
      ∧ decode ?x
  -> ∃?wt/1.
      ∃?wu.
        ⊤
        ∧ ∃?warr.
          ∃?wt.
            ∃?x.
              ∃?final_type.
                hole
                {
                  final_type = wt/1 -> wt
                  warr |--> final_type
                  wt
                  wu
                  x |--> wt/1
                  wt/1 = wu -> wt
                }
                (?wt/1 = ?x)
              ∧ decode ?final_type
        ∧ decode ?x
    ∧ ?wu = ?x
  
  File "selfapp.test", line 1, characters 0-13:
  Error:
    cycle on constraint variable ?wu
  





## Erroneous programs

  $ minihell $FLAGS --log-solver error_undefined_var.test
  File "error_undefined_var.test", line 1, characters 9-23:
  Fatal error: exception Invalid_argument("Constraint variable 'id' is unbound at this point")
  [2]



## Generator tests

This gives example outputs for my implementation. It is completely
fine if your own implementation produces different (sensible) results.

There are not many programs with size 3, 4 and 5.

  $ minigen --search exhaustive --types --size 2 --count 100
  lambda (x/5 : α/1). x/5
  
  Inferred type : α/1 -> α/1

  $ minigen --search exhaustive --types --size 3 --count 100
  lambda (v/14 : α/7). lambda (u/19 : β/7). v/14
  
  Inferred type : α/7 -> β/7 -> α/7
  
  
  
  lambda (v/14 : δ/7). lambda (u/19 : γ/7). u/19
  
  Inferred type : δ/7 -> γ/7 -> γ/7

  $ minigen --search exhaustive --types --size 4 --count 100
  lambda
  (v/6a : δ/2a). lambda (w/86 : β/2b). lambda (z/8c : α/2b). v/6a
  
  Inferred type : δ/2a -> β/2b -> α/2b -> δ/2a
  
  
  
  lambda
  (v/6a : α/2c). lambda (w/86 : γ/2b). lambda (z/8c : δ/2b). w/86
  
  Inferred type : α/2c -> γ/2b -> δ/2b -> γ/2b
  
  
  
  lambda
  (v/6a : δ/2c). lambda (w/86 : γ/2c). lambda (z/8c : β/2c). z/8c
  
  Inferred type : δ/2c -> γ/2c -> β/2c -> β/2c
  
  
  
  lambda (v/6a : α/32). let (z/a0 : α/32) = v/6a in v/6a
  
  Inferred type : α/32 -> α/32
  
  
  
  lambda (v/6a : β/32). let (z/a0 : β/32) = v/6a in z/a0
  
  Inferred type : β/32 -> β/32
  
  
  
  lambda (v/6a : γ/39). (v/6a, v/6a)
  
  Inferred type : γ/39 -> {γ/39 * γ/39}
  
  
  
  lambda
  (v/6a : {δ/40 * α/41}).
    let ((u/d8 : δ/40), (v/d8 : α/41)) = v/6a in v/6a
  
  Inferred type : {δ/40 * α/41} -> {δ/40 * α/41}
  
  
  
  lambda
  (v/6a : {β/41 * γ/41}).
    let ((u/d8 : β/41), (v/d8 : γ/41)) = v/6a in u/d8
  
  Inferred type : {β/41 * γ/41} -> β/41
  
  
  
  lambda
  (v/6a : {α/42 * δ/41}).
    let ((u/d8 : α/42), (v/d8 : δ/41)) = v/6a in v/d8
  
  Inferred type : {α/42 * δ/41} -> δ/41
  
  
  
  let (u/ef : β/53 -> β/53) = lambda (z/104 : β/53). z/104 in u/ef
  
  Inferred type : β/53 -> β/53

An example of random sampling output at higher size.

  $ minigen --seed 42 --types --size 6 --count 10
  lambda
  (z/1 : δ/bd).
    lambda
    (x/2a : {β/bd * γ/bd}).
      lambda
      (u/7a : α/bd). let ((x/290 : β/bd), (y/290 : γ/bd)) = x/2a in u/7a
  
  Inferred type : δ/bd -> {β/bd * γ/bd} -> α/bd -> α/bd
  
  
  
  lambda
  (z/1 : β/f9).
    (lambda (x/345 : α/f9). lambda (y/345 : δ/f8). y/345, z/1)
  
  Inferred type : β/f9 -> {α/f9 -> δ/f8 -> δ/f8 * β/f9}
  
  
  
  lambda
  (z/1 : {γ/fa * β/fb}).
    let ((u/34f : γ/fa), (v/34f : β/fb)) = z/1 in
      lambda (w/34f : α/fb). lambda (x/351 : δ/fa). u/34f
  
  Inferred type : {γ/fa * β/fb} -> α/fb -> δ/fa -> γ/fa
  
  
  
  (
    lambda (u/3e2 : β/128). lambda (v/3e2 : γ/128). u/3e2,
    lambda (w/3e2 : δ/128). w/3e2
  )
  
  Inferred type : {β/128 -> γ/128 -> β/128 * δ/128 -> δ/128}
  
  
  
  lambda
  (z/1 : α/14a). let (x/1a : {α/14a * α/14a}) = (z/1, z/1) in x/1a
  
  Inferred type : α/14a -> {α/14a * α/14a}
  
  
  
  lambda
  (z/1 : {{β/14f * γ/14f} * δ/14f}).
    let ((u/456 : {β/14f * γ/14f}), (v/456 : δ/14f)) = z/1 in
      let ((w/456 : β/14f), (x/458 : γ/14f)) = u/456 in w/456
  
  Inferred type : {{β/14f * γ/14f} * δ/14f} -> β/14f
  
  
  
  lambda (z/1 : β/169). ((z/1, z/1), z/1)
  
  Inferred type : β/169 -> {{β/169 * β/169} * β/169}
  
  
  
  lambda
  (z/1 : {γ/16e * δ/16e}).
    let ((x/4b3 : γ/16e), (y/4b3 : δ/16e)) = z/1 in
      let (z/4b2 : γ/16e) = x/4b3 in z/1
  
  Inferred type : {γ/16e * δ/16e} -> {γ/16e * δ/16e}
  
  
  
  let (w/4 : α/171 -> γ/171 -> β/171 -> α/171) =
    lambda
    (y/31 : α/171). lambda (z/30 : γ/171). lambda (x/81 : β/171). y/31
  in
    w/4
  
  Inferred type : α/171 -> γ/171 -> β/171 -> α/171
  
  
  
  lambda
  (z/1 : α/2bf). let (x/1a : {α/2bf * α/2bf}) = (z/1, z/1) in z/1
  
  Inferred type : α/2bf -> α/2bf
