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
    ∃final_type.
      (∃x wt (warr = x -> wt). final_type = warr ∧ decode x ∧ wt = x)
      ∧ decode final_type
  
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
    ∃final_type.
      (∃x wt (warr = x -> wt).
        final_type = warr ∧ decode x ∧ (∃(int = int). int = x ∧ int = wt))
      ∧ decode final_type
  
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
    ∃final_type.
      (∃x wt (warr = x -> wt).
        final_type = warr ∧ decode x ∧ (∃(int = int). int = x ∧ int = wt))
      ∧ decode final_type
  
  Constraint solving log:
    ∃final_type.
      decode final_type
      ∧ (∃x wt (warr = x -> wt).
        (∃(int = int). int = wt ∧ int = x) ∧ decode x ∧ final_type = warr)
    ∃final_type.
      decode final_type
      ∧ (∃x wt (warr = x -> wt).
        (∃(int = int). int = wt ∧ int = x) ∧ decode x ∧ final_type = warr)
    ∃x final_type.
      decode final_type
      ∧ (∃wt (warr = x -> wt).
        (∃(int = int). int = wt ∧ int = x) ∧ decode x ∧ final_type = warr)
    ∃x wt final_type.
      decode final_type
      ∧ (∃(warr = x -> wt).
        (∃(int = int). int = wt ∧ int = x) ∧ decode x ∧ final_type = warr)
    ∃x wt (warr = x -> wt) final_type.
      decode final_type
      ∧ (∃(int = int). int = wt ∧ int = x)
      ∧ decode x
      ∧ final_type = warr
    ∃x wt (final_type = x -> wt).
      decode final_type ∧ (∃(int = int). int = wt ∧ int = x) ∧ decode x
    ∃x wt (int = int) (final_type = x -> wt).
      decode final_type ∧ int = wt ∧ int = x ∧ decode x
    ∃wt (int = int) (final_type = int -> wt).
      decode final_type ∧ int = wt ∧ decode int
    ∃(int = int) (final_type = int -> int). decode final_type ∧ decode int
  
  Inferred type:
    int -> int
  
  Elaborated term:
    lambda (x : int). x
  

## An erroneous program

  $ minihell $FLAGS error.test
  Input term:
    (lambda x. (x : int)) (lambda y. y)
  
  Generated constraint:
    ∃final_type.
      (∃wu (wt = wu -> final_type).
        (∃x wt/1 (warr = x -> wt/1).
          wt = warr ∧ decode x ∧ (∃(int = int). int = x ∧ int = wt/1))
        ∧ (∃y wt/2 (warr/1 = y -> wt/2). wu = warr/1 ∧ decode y ∧ wt/2 = y))
      ∧ decode final_type
  
  Error:
      int
    incompatible with
      β -> α
  

## Examples with products

  $ minihell $FLAGS curry.test
  Input term:
    lambda f. lambda x. lambda y. f (x, y)
  
  Generated constraint:
    ∃final_type.
      (∃f wt (warr = f -> wt).
        final_type = warr
        ∧ decode f
        ∧ (∃x wt/1 (warr/1 = x -> wt/1).
          wt = warr/1
          ∧ decode x
          ∧ (∃y wt/2 (warr/2 = y -> wt/2).
            wt/1 = warr/2
            ∧ decode y
            ∧ (∃wu (wt/3 = wu -> wt/2).
              wt/3 = f
              ∧ (∃w1. w1 = x ∧ (∃w2. w2 = y ∧ (∃(wprod = {w1 * w2}). wu = wprod)))))))
      ∧ decode final_type
  
  Inferred type:
    ({γ * β} -> α) -> γ -> β -> α
  
  Elaborated term:
    lambda (f : {γ * β} -> α). lambda (x : γ). lambda (y : β). f (x, y)
  

  $ minihell $FLAGS uncurry.test
  Input term:
    lambda f. lambda p. let (x, y) = p in f x y
  
  Generated constraint:
    ∃final_type.
      (∃f wt (warr = f -> wt).
        final_type = warr
        ∧ decode f
        ∧ (∃p wt/1 (warr/1 = p -> wt/1).
          wt = warr/1
          ∧ decode p
          ∧ (∃x y (wt/2 = {x * y}).
            decode x
            ∧ decode y
            ∧ wt/2 = p
            ∧ (∃wu (wt/3 = wu -> wt/1).
              (∃wu/1 (wt/4 = wu/1 -> wt/3). wt/4 = f ∧ wu/1 = x) ∧ wu = y))))
      ∧ decode final_type
  
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
    ∃final_type.
      (∃x wt (warr = x -> wt).
        final_type = warr
        ∧ decode x
        ∧ (∃wu (wt/1 = wu -> wt). wt/1 = x ∧ wu = x))
      ∧ decode final_type
  
  Constraint solving log:
    ∃final_type.
      decode final_type
      ∧ (∃x wt (warr = x -> wt).
        (∃wu (wt/1 = wu -> wt). wu = x ∧ wt/1 = x)
        ∧ decode x
        ∧ final_type = warr)
    ∃final_type.
      decode final_type
      ∧ (∃x wt (warr = x -> wt).
        (∃wu (wt/1 = wu -> wt). wu = x ∧ wt/1 = x)
        ∧ decode x
        ∧ final_type = warr)
    ∃x final_type.
      decode final_type
      ∧ (∃wt (warr = x -> wt).
        (∃wu (wt/1 = wu -> wt). wu = x ∧ wt/1 = x)
        ∧ decode x
        ∧ final_type = warr)
    ∃x wt final_type.
      decode final_type
      ∧ (∃(warr = x -> wt).
        (∃wu (wt/1 = wu -> wt). wu = x ∧ wt/1 = x)
        ∧ decode x
        ∧ final_type = warr)
    ∃x wt (warr = x -> wt) final_type.
      decode final_type
      ∧ (∃wu (wt/1 = wu -> wt). wu = x ∧ wt/1 = x)
      ∧ decode x
      ∧ final_type = warr
    ∃x wt (final_type = x -> wt).
      decode final_type
      ∧ (∃wu (wt/1 = wu -> wt). wu = x ∧ wt/1 = x)
      ∧ decode x
    ∃x wu wt (final_type = x -> wt).
      decode final_type ∧ (∃(wt/1 = wu -> wt). wu = x ∧ wt/1 = x) ∧ decode x
    ∃x wu wt (wt/1 = wu -> wt) wt (final_type = x -> wt).
      decode final_type ∧ wu = x ∧ wt/1 = x ∧ decode x
    ∃wu wt (wt/1 = wu -> wt) wt (final_type = wt/1 -> wt).
      decode final_type ∧ ⊥ ∧ decode wt/1
  
  Error:
    cycle on constraint variable
    wu
  
## Generator tests

This gives example outputs for my implementation. It is completely
fine if your own implementation produces different (sensible) results.

There are not many programs with depth 3, 4 and 5.

  $ minigen --exhaustive --depth 3 --count 100
  lambda (z/3 : α/1). z/3

  $ minigen --exhaustive --depth 4 --count 100
  lambda (x/10 : α/4). lambda (y/14 : δ/3). x/10
  
  lambda (x/10 : β/4). lambda (y/14 : α/4). y/14

An example of random sampling output at higher depth.

  $ minigen --seed 42 --depth 10 --count 10
  lambda
  (x/5cb : α/355).
    lambda
    (y/5cb : β/354 -> δ/354).
      lambda (z/5cb : γ/354). lambda (u/5cb : β/354). (z/5cb, y/5cb u/5cb)
  
  let
  (v/933 : δ/524 -> δ/524)
  =
  let (w/933 : δ/524 -> δ/524) = lambda (x/934 : δ/524). x/934 in w/933
  in (v/933, lambda (y/934 : γ/524). y/934)
  
  lambda
  (w/a85 : {δ/5ca * γ/5ca}).
    let
    (x/a86 : {δ/5ca * γ/5ca})
    =
    w/a85
    in
      (lambda (y/a86 : δ/5ca). y/a86)
        (let ((z/a86 : δ/5ca), (u/a86 : γ/5ca)) = w/a85 in z/a86)
  
  lambda
  (z/da4 : α/765).
    let
    (u/da4 : α/765)
    =
    z/da4
    in
      lambda
      (v/da4 : δ/764).
        lambda (w/da4 : γ/764). (lambda (x/da5 : β/764). v/da4, v/da4)
  
  lambda
  (u/1568 : {α/ba4 * δ/ba3}).
    (
      (u/1568, let ((v/1568 : α/ba4), (w/1568 : δ/ba3)) = u/1568 in v/1568),
      lambda (x/1569 : γ/ba3). u/1568
    )
  
  lambda
  (y/2746 : {γ/1527 * β/1527}).
    lambda
    (z/2746 : α/1527).
      (let
      ((u/2746 : γ/1527), (v/2746 : β/1527))
      =
      y/2746
      in lambda (w/2746 : α/1527). lambda (x/2747 : δ/1526). x/2747)
        z/2746
  
  let
  (
    (v/3113 : α/1a59 -> δ/1a58 -> α/1a59),
    (w/3113 : γ/1a58 -> γ/1a58)
  )
  =
  (
    lambda (x/3114 : α/1a59). lambda (y/3114 : δ/1a58). x/3114,
    lambda (z/3114 : γ/1a58). z/3114
  )
  in lambda (u/3114 : β/1a58). u/3114
  
  lambda
  (z/391f : γ/1e91).
    lambda
    (u/391f : α/1e91).
      let
      (v/391f : α/1e91)
      =
      let
      (w/391f : β/1e91 -> β/1e91)
      =
      lambda (x/3920 : β/1e91). x/3920
      in u/391f
      in lambda (y/3920 : δ/1e90). u/391f
  
  (
    lambda (x/3c59 : α/2052). lambda (y/3c59 : δ/2051). y/3c59,
    lambda
    (z/3c59 : γ/2051).
      lambda
      (u/3c59 : {β/2051 * α/2051}).
        let ((v/3c59 : β/2051), (w/3c59 : α/2051)) = u/3c59 in z/3c59
  )
  
  lambda
  (w/5708 : δ/2e9e).
    lambda
    (x/5709 : γ/2e9f).
      let
      (y/5709 : β/2e9f -> α/2e9f -> δ/2e9e)
      =
      lambda (z/5709 : β/2e9f). lambda (u/5709 : α/2e9f). w/5708
      in (x/5709, y/5709)
