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
    fun x -> x
  
  Generated constraint:
    ∃final_type.
      (∃x wt (warr = x -> wt). final_type = warr ∧ decode x ∧ wt = x)
      ∧ decode final_type
  
  Inferred type:
    α/1 -> α/1
  
  Elaborated term:
    fun (x : α) -> x
  

`id_int` is the monomorphic identity on the type `int`. Note
that we have not implemented support for a built-in `int`
type, this is just an abstract/rigid type variable: `Constr
(Var ...)` at type `STLC.ty`.

  $ minihell $FLAGS id_int.test
  Input term:
    fun x -> (x : int)
  
  Generated constraint:
    ∃final_type.
      (∃x wt (warr = x -> wt).
        final_type = warr ∧ decode x ∧ (∃(int = int). int = x ∧ int = wt))
      ∧ decode final_type
  
  Inferred type:
    int -> int
  
  Elaborated term:
    fun (x : α) -> x
  

## Logging the constraint-solving process

You can ask `minihell` to show how the constraint evolves as
the solver progresses and accumulates more information on
the inference variables.

  $ minihell $FLAGS --log-solver id_int.test
  Input term:
    fun x -> (x : int)
  
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
    fun (x : α) -> x
  

## An erroneous program

  $ minihell $FLAGS error.test
  Input term:
    (fun x -> (x : int)) (fun y -> y)
  
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
      α/2 -> α/1
  

## Examples with products

  $ minihell $FLAGS curry.test
  Input term:
    fun f -> fun x -> fun y -> f (x, y)
  
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
              ∧ (∃w1. w1 = x ∧ (∃w2. w2 = y ∧ (∃(wprod = (w1 * w2)). wu = wprod)))))))
      ∧ decode final_type
  
  Inferred type:
    ((α/5 * α/4) -> α/3) -> α/5 -> α/4 -> α/3
  
  Elaborated term:
    fun (f : α) -> fun (x : α/1) -> fun (y : α/2) -> f (x, y)
  

  $ minihell $FLAGS uncurry.test
  Input term:
    fun f -> fun p -> let (x, y) = p in f x y
  
  Generated constraint:
    ∃final_type.
      (∃f wt (warr = f -> wt).
        final_type = warr
        ∧ decode f
        ∧ (∃p wt/1 (warr/1 = p -> wt/1).
          wt = warr/1
          ∧ decode p
          ∧ (∃x y (wt/2 = (x * y)).
            decode x
            ∧ decode y
            ∧ wt/2 = p
            ∧ (∃wu (wt/3 = wu -> wt/1).
              (∃wu/1 (wt/4 = wu/1 -> wt/3). wt/4 = f ∧ wu/1 = x) ∧ wu = y))))
      ∧ decode final_type
  
  Inferred type:
    (α/5 -> α/6 -> α/4) -> (α/5 * α/6) -> α/4
  
  Elaborated term:
    fun
    (f : α)
    -> fun (p : α/1) -> let ((x : α/2), (y : α/3)) = p in f x y
  
## Cyclic types

Unification can sometimes create cyclic types. We decide to reject
these situations with an error. (We could also accept those as they
preserve type-safety, but they have the issue, just like the
OCaml -rectypes option, that they allow to write somewhat-nonsensical
program, and our random term generator will be very good at finding
a lot of those.)

  $ minihell $FLAGS --log-solver selfapp.test
  Input term:
    fun x -> x x
  
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
      decode final_type ∧ wu = wt/1 ∧ decode wt/1
    ∃wt (wu = wu -> wt) wt (final_type = wu -> wt).
      decode final_type ∧ decode wu
  
  Error:
    cycle on constraint variable
    wu
  
