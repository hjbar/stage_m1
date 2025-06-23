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
    let ?scheme_final_scheme : ?final_term =
      ∃?x ?wt (?warr = ?x -> ?wt). ?final_term = ?warr ∧ ?wt = ?x ∧ decode ?x
    in decode_scheme ?scheme_final_scheme
  
  Inferred type:
    ∀α. α -> α
  
  Elaborated term:
    lambda (x : α). x
  




  $ minihell $FLAGS id_poly_with_let.test
  Input term:
    lambda y. let id = lambda x. x in id y
  
  Generated constraint:
    let ?scheme_final_scheme : ?final_term =
      ∃?y ?wt (?warr = ?y -> ?wt).
        ?final_term = ?warr
        ∧ (let ?scheme_s : ?id =
          ∃?x ?wt/2 (?warr/1 = ?x -> ?wt/2).
            ?id = ?warr/1 ∧ ?wt/2 = ?x ∧ decode ?x
        in
          ((∃?wu (?wt/1 = ?wu -> ?wt). ?scheme_s ≤ ?wt/1 ∧ ?wu = ?y)
          ∧ decode_scheme ?scheme_s))
        ∧ decode ?y
    in decode_scheme ?scheme_final_scheme
  
  Inferred type:
    ∀α. α -> α
  
  Elaborated term:
    lambda
    (y : α). let Λβ. (id : β -> β) = lambda (x : β). x in id[α] y
  




`id_int` is the monomorphic identity on the type `int`. Note
that we have not implemented support for a built-in `int`
type, this is just an abstract/rigid type variable: `Constr
(Var ...)` at type `F.ty`.

  $ minihell $FLAGS id_int.test
  Input term:
    lambda x. (x : int)
  
  Generated constraint:
    let ?scheme_final_scheme : ?final_term =
      ∃?x ?wt (?warr = ?x -> ?wt).
        ?final_term = ?warr
        ∧ (∃(?int = int). ?int = ?wt ∧ ?int = ?x)
        ∧ decode ?x
    in decode_scheme ?scheme_final_scheme
  
  Inferred type:
    int -> int
  
  Elaborated term:
    lambda (x : int). (x : int)
  




  $ minihell $FLAGS let_poly.test
  Input term:
    let x = lambda y. y in x
  
  Generated constraint:
    let ?scheme_final_scheme : ?final_term =
      let ?scheme_s : ?x =
        ∃?y ?wt (?warr = ?y -> ?wt). ?x = ?warr ∧ ?wt = ?y ∧ decode ?y
      in (?scheme_s ≤ ?final_term ∧ decode_scheme ?scheme_s)
    in decode_scheme ?scheme_final_scheme
  
  Inferred type:
    ∀α. α -> α
  
  Elaborated term:
    let Λβ. (x : β -> β) = lambda (y : β). y in x[α]
  






## Examples with products

  $ minihell $FLAGS curry.test
  Input term:
    lambda f. lambda x. lambda y. f (x, y)
  
  Generated constraint:
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
    in decode_scheme ?scheme_final_scheme
  
  Inferred type:
    ∀γ. ∀β. ∀α. ({γ * β} -> α) -> γ -> β -> α
  
  Elaborated term:
    lambda (f : {γ * β} -> α). lambda (x : γ). lambda (y : β). f (x, y)
  




  $ minihell $FLAGS uncurry.test
  Input term:
    lambda f. lambda p. let (x, y) = p in f x y
  
  Generated constraint:
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
    in decode_scheme ?scheme_final_scheme
  
  Inferred type:
    ∀β. ∀γ. ∀α. (β -> γ -> α) -> {β * γ} -> α
  
  Elaborated term:
    lambda
    (f : β -> γ -> α).
      lambda (p : {β * γ}). let ((x : β), (y : γ)) = p in f x y
  






## Polymorhpism

  $ minihell $FLAGS poly_easy_use.test
  Input term:
    lambda a. let id = lambda x. x in let r = id a in r
  
  Generated constraint:
    let ?scheme_final_scheme : ?final_term =
      ∃?a ?wt (?warr = ?a -> ?wt).
        ?final_term = ?warr
        ∧ (let ?scheme_s : ?id =
          ∃?x ?wt/2 (?warr/1 = ?x -> ?wt/2).
            ?id = ?warr/1 ∧ ?wt/2 = ?x ∧ decode ?x
        in
          ((let ?scheme_s/1 : ?r =
            ∃?wu (?wt/1 = ?wu -> ?r). ?scheme_s ≤ ?wt/1 ∧ ?wu = ?a
          in (?scheme_s/1 ≤ ?wt ∧ decode_scheme ?scheme_s/1))
          ∧ decode_scheme ?scheme_s))
        ∧ decode ?a
    in decode_scheme ?scheme_final_scheme
  
  Inferred type:
    ∀α. α -> α
  
  Elaborated term:
    lambda
    (a : α).
      let Λβ. (id : β -> β) = lambda (x : β). x in
        let (r : α) = id[α] a in r
  




  $ minihell $FLAGS poly_use.test
  Input term:
    let id = lambda x. x in
      lambda a. lambda b. let l = id a in let r = id b in (l, r)
  
  Generated constraint:
    let ?scheme_final_scheme : ?final_term =
      let ?scheme_s : ?id =
        ∃?x ?wt/4 (?warr/2 = ?x -> ?wt/4).
          ?id = ?warr/2 ∧ ?wt/4 = ?x ∧ decode ?x
      in
        ((∃?a ?wt (?warr = ?a -> ?wt).
          ?final_term = ?warr
          ∧ (∃?b ?wt/1 (?warr/1 = ?b -> ?wt/1).
            ?wt = ?warr/1
            ∧ (let ?scheme_s/1 : ?l =
              ∃?wu/1 (?wt/3 = ?wu/1 -> ?l). ?scheme_s ≤ ?wt/3 ∧ ?wu/1 = ?a
            in
              ((let ?scheme_s/2 : ?r =
                ∃?wu (?wt/2 = ?wu -> ?r). ?scheme_s ≤ ?wt/2 ∧ ?wu = ?b
              in
                ((∃?w1.
                  ?scheme_s/1 ≤ ?w1
                  ∧ (∃?w2.
                    ?scheme_s/2 ≤ ?w2 ∧ (∃(?wprod = {?w1 * ?w2}). ?wt/1 = ?wprod)))
                ∧ decode_scheme ?scheme_s/2))
              ∧ decode_scheme ?scheme_s/1))
            ∧ decode ?b)
          ∧ decode ?a)
        ∧ decode_scheme ?scheme_s)
    in decode_scheme ?scheme_final_scheme
  
  Inferred type:
    ∀α. ∀β. α -> β -> {α * β}
  
  Elaborated term:
    let Λγ. (id : γ -> γ) = lambda (x : γ). x in
      lambda
      (a : α).
        lambda
        (b : β). let (l : α) = id[α] a in let (r : β) = id[β] b in (l, r)
  





  $ minihell $FLAGS poly_annot_use.test
  Input term:
    let id = lambda x. x in
      lambda
      a. lambda b. let l = id a in let r = id b in ((l : int), (r : bool))
  
  Generated constraint:
    let ?scheme_final_scheme : ?final_term =
      let ?scheme_s : ?id =
        ∃?x ?wt/4 (?warr/2 = ?x -> ?wt/4).
          ?id = ?warr/2 ∧ ?wt/4 = ?x ∧ decode ?x
      in
        ((∃?a ?wt (?warr = ?a -> ?wt).
          ?final_term = ?warr
          ∧ (∃?b ?wt/1 (?warr/1 = ?b -> ?wt/1).
            ?wt = ?warr/1
            ∧ (let ?scheme_s/1 : ?l =
              ∃?wu/1 (?wt/3 = ?wu/1 -> ?l). ?scheme_s ≤ ?wt/3 ∧ ?wu/1 = ?a
            in
              ((let ?scheme_s/2 : ?r =
                ∃?wu (?wt/2 = ?wu -> ?r). ?scheme_s ≤ ?wt/2 ∧ ?wu = ?b
              in
                ((∃?w1.
                  (∃(?int = int). ?int = ?w1 ∧ ?scheme_s/1 ≤ ?int)
                  ∧ (∃?w2.
                    (∃(?bool = bool). ?bool = ?w2 ∧ ?scheme_s/2 ≤ ?bool)
                    ∧ (∃(?wprod = {?w1 * ?w2}). ?wt/1 = ?wprod)))
                ∧ decode_scheme ?scheme_s/2))
              ∧ decode_scheme ?scheme_s/1))
            ∧ decode ?b)
          ∧ decode ?a)
        ∧ decode_scheme ?scheme_s)
    in decode_scheme ?scheme_final_scheme
  
  Inferred type:
    int -> bool -> {int * bool}
  
  Elaborated term:
    let Λα. (id : α -> α) = lambda (x : α). x in
      lambda
      (a : int).
        lambda
        (b : bool).
          let (l : int) = id[int] a in
            let (r : bool) = id[bool] b in ((l : int), (r : bool))
  






## Logging the constraint-solving process

You can ask `minihell` to show how the constraint evolves as
the solver progresses and accumulates more information on
the inference variables.

  $ minihell $FLAGS --log-solver id_int.test
  Input term:
    lambda x. (x : int)
  
  Generated constraint:
    let ?scheme_final_scheme : ?final_term =
      ∃?x ?wt (?warr = ?x -> ?wt).
        ?final_term = ?warr
        ∧ (∃(?int = int). ?int = ?wt ∧ ?int = ?x)
        ∧ decode ?x
    in decode_scheme ?scheme_final_scheme
  
  Constraint solving log:
  -> hole {}
    (
      let ?scheme_final_scheme : ?final_term =
        ∃?x ?wt (?warr = ?x -> ?wt).
          ?final_term = ?warr
          ∧ (∃(?int = int). ?int = ?wt ∧ ?int = ?x)
          ∧ decode ?x
      in decode_scheme ?scheme_final_scheme
    )
  -> hole
    {
      Env :
        final_term(0)
      Pool :
        0 |-->
           final_term(0)
    }
    (
      let ?scheme_final_scheme : ?final_term =
        ∃?x ?wt (?warr = ?x -> ?wt).
          ?final_term = ?warr
          ∧ (∃(?int = int). ?int = ?wt ∧ ?int = ?x)
          ∧ decode ?x
      in decode_scheme ?scheme_final_scheme
    )
  -> let ?scheme_final_scheme : ?final_term =
      hole
      {
        Env :
          final_term(0)
          x(0)
        Pool :
          0 |-->
             x(0)
             final_term(0)
      }
      (
        ∃?wt (?warr = ?x -> ?wt).
          ?final_term = ?warr
          ∧ (∃(?int = int). ?int = ?wt ∧ ?int = ?x)
          ∧ decode ?x
      )
    in decode_scheme ?scheme_final_scheme
  -> ∃?x.
      let ?scheme_final_scheme : ?final_term =
        hole
        {
          Env :
            final_term(0)
            wt(0)
            x(0)
          Pool :
            0 |-->
               wt(0)
               x(0)
               final_term(0)
        }
        (
          ∃(?warr = ?x -> ?wt).
            ?final_term = ?warr
            ∧ (∃(?int = int). ?int = ?wt ∧ ?int = ?x)
            ∧ decode ?x
        )
      in decode_scheme ?scheme_final_scheme
  -> ∃?wt.
      ∃?x.
        let ?scheme_final_scheme : ?final_term =
          hole
          {
            Env :
              final_term(0)
              warr(0) = x -> wt
              wt(0)
              x(0)
            Pool :
              0 |-->
                 warr(0) = x -> wt
                 wt(0)
                 x(0)
                 final_term(0)
          }
          (
            ?final_term = ?warr
            ∧ (∃(?int = int). ?int = ?wt ∧ ?int = ?x)
            ∧ decode ?x
          )
        in decode_scheme ?scheme_final_scheme
  -> ∃?warr.
      ∃?wt.
        ∃?x.
          let ?scheme_final_scheme : ?final_term =
            hole
            {
              Env :
                warr |--> final_term
                final_term(0) = x -> wt
                wt(0)
                x(0)
              Pool :
                0 |-->
                   warr |--> final_term
                   wt(0)
                   x(0)
                   final_term(0) = x -> wt
            }
            (?final_term = ?warr)
          in decode_scheme ?scheme_final_scheme
    ∧ decode ?x
    ∧ ∃(?int = int). ?int = ?wt ∧ ?int = ?x
  -> ⊤
    ∧ ∃?warr.
      ∃?wt.
        ∃?x.
          let ?scheme_final_scheme : ?final_term =
            hole
            {
              Env :
                warr |--> final_term
                final_term(0) = x -> wt
                int(0) = int
                wt(0)
                x(0)
              Pool :
                0 |-->
                   warr |--> final_term
                   int(0) = int
                   wt(0)
                   x(0)
                   final_term(0) = x -> wt
            }
            (?int = ?wt ∧ ?int = ?x)
          in decode_scheme ?scheme_final_scheme
    ∧ decode ?x
  -> ∃?int.
      ⊤
      ∧ ∃?warr.
        ∃?wt.
          ∃?x.
            let ?scheme_final_scheme : ?final_term =
              hole
              {
                Env :
                  warr |--> final_term
                  wt |--> int
                  final_term(0) = x -> int
                  int(0) = int
                  x(0)
                Pool :
                  0 |-->
                     warr |--> final_term
                     wt |--> int
                     int(0) = int
                     x(0)
                     final_term(0) = x -> int
              }
              (?int = ?wt)
            in decode_scheme ?scheme_final_scheme
      ∧ decode ?x
    ∧ ?int = ?x
  -> ⊤
    ∧ ∃?int.
      ⊤
      ∧ ∃?warr.
        ∃?wt.
          ∃?x.
            let ?scheme_final_scheme : ?final_term =
              hole
              {
                Env :
                  warr |--> final_term
                  wt |--> int
                  x |--> int
                  final_term(0) = int -> int
                  int(0) = int
                Pool :
                  0 |-->
                     warr |--> final_term
                     wt |--> int
                     x |--> int
                     int(0) = int
                     final_term(0) = int -> int
              }
              (?int = ?x)
            in decode_scheme ?scheme_final_scheme
      ∧ decode ?x
  <- let ?scheme_final_scheme : ?final_term =
      hole
      {
        Schemes :
          final_scheme: final_term [final_term int]
        Env :
          warr |--> final_term
          wt |--> int
          x |--> int
          final_term(G) = int -> int
          int(G) = int
  
      }
      (⊤)
    in decode_scheme ?scheme_final_scheme
  
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
    let ?scheme_final_scheme : ?final_term =
      ∃?wu (?wt = ?wu -> ?final_term).
        (∃?x ?wt/1 (?warr = ?x -> ?wt/1).
          ?wt = ?warr ∧ (∃(?int = int). ?int = ?wt/1 ∧ ?int = ?x) ∧ decode ?x)
        ∧ (∃?y ?wt/2 (?warr/1 = ?y -> ?wt/2).
          ?wu = ?warr/1 ∧ ?wt/2 = ?y ∧ decode ?y)
    in decode_scheme ?scheme_final_scheme
  
  Constraint solving log:
  -> hole {}
    (
      let ?scheme_final_scheme : ?final_term =
        ∃?wu (?wt = ?wu -> ?final_term).
          (∃?x ?wt/1 (?warr = ?x -> ?wt/1).
            ?wt = ?warr ∧ (∃(?int = int). ?int = ?wt/1 ∧ ?int = ?x) ∧ decode ?x)
          ∧ (∃?y ?wt/2 (?warr/1 = ?y -> ?wt/2).
            ?wu = ?warr/1 ∧ ?wt/2 = ?y ∧ decode ?y)
      in decode_scheme ?scheme_final_scheme
    )
  -> hole
    {
      Env :
        final_term(0)
      Pool :
        0 |-->
           final_term(0)
    }
    (
      let ?scheme_final_scheme : ?final_term =
        ∃?wu (?wt = ?wu -> ?final_term).
          (∃?x ?wt/1 (?warr = ?x -> ?wt/1).
            ?wt = ?warr ∧ (∃(?int = int). ?int = ?wt/1 ∧ ?int = ?x) ∧ decode ?x)
          ∧ (∃?y ?wt/2 (?warr/1 = ?y -> ?wt/2).
            ?wu = ?warr/1 ∧ ?wt/2 = ?y ∧ decode ?y)
      in decode_scheme ?scheme_final_scheme
    )
  -> let ?scheme_final_scheme : ?final_term =
      hole
      {
        Env :
          final_term(0)
          wu(0)
        Pool :
          0 |-->
             wu(0)
             final_term(0)
      }
      (
        ∃(?wt = ?wu -> ?final_term).
          (∃?x ?wt/1 (?warr = ?x -> ?wt/1).
            ?wt = ?warr ∧ (∃(?int = int). ?int = ?wt/1 ∧ ?int = ?x) ∧ decode ?x)
          ∧ (∃?y ?wt/2 (?warr/1 = ?y -> ?wt/2).
            ?wu = ?warr/1 ∧ ?wt/2 = ?y ∧ decode ?y)
      )
    in decode_scheme ?scheme_final_scheme
  -> ∃?wu.
      let ?scheme_final_scheme : ?final_term =
        hole
        {
          Env :
            final_term(0)
            wt(0) = wu -> final_term
            wu(0)
          Pool :
            0 |-->
               wt(0) = wu -> final_term
               wu(0)
               final_term(0)
        }
        (
          (∃?x ?wt/1 (?warr = ?x -> ?wt/1).
            ?wt = ?warr ∧ (∃(?int = int). ?int = ?wt/1 ∧ ?int = ?x) ∧ decode ?x)
          ∧ (∃?y ?wt/2 (?warr/1 = ?y -> ?wt/2).
            ?wu = ?warr/1 ∧ ?wt/2 = ?y ∧ decode ?y)
        )
      in decode_scheme ?scheme_final_scheme
  -> ∃?wt.
      ∃?wu.
        let ?scheme_final_scheme : ?final_term =
          hole
          {
            Env :
              final_term(0)
              wt(0) = wu -> final_term
              wu(0)
              x(0)
            Pool :
              0 |-->
                 x(0)
                 wt(0) = wu -> final_term
                 wu(0)
                 final_term(0)
          }
          (
            ∃?wt/1 (?warr = ?x -> ?wt/1).
              ?wt = ?warr ∧ (∃(?int = int). ?int = ?wt/1 ∧ ?int = ?x) ∧ decode ?x
          )
        in decode_scheme ?scheme_final_scheme
    ∧ ∃?y ?wt/2 (?warr/1 = ?y -> ?wt/2).
      ?wu = ?warr/1 ∧ ?wt/2 = ?y ∧ decode ?y
  -> ∃?x.
      ∃?wt.
        ∃?wu.
          let ?scheme_final_scheme : ?final_term =
            hole
            {
              Env :
                final_term(0)
                wt(0) = wu -> final_term
                wu(0)
                x(0)
                wt/1(0)
              Pool :
                0 |-->
                   wt/1(0)
                   x(0)
                   wt(0) = wu -> final_term
                   wu(0)
                   final_term(0)
            }
            (
              ∃(?warr = ?x -> ?wt/1).
                ?wt = ?warr
                ∧ (∃(?int = int). ?int = ?wt/1 ∧ ?int = ?x)
                ∧ decode ?x
            )
          in decode_scheme ?scheme_final_scheme
      ∧ ∃?y ?wt/2 (?warr/1 = ?y -> ?wt/2).
        ?wu = ?warr/1 ∧ ?wt/2 = ?y ∧ decode ?y
  -> ∃?wt/1.
      ∃?x.
        ∃?wt.
          ∃?wu.
            let ?scheme_final_scheme : ?final_term =
              hole
              {
                Env :
                  final_term(0)
                  warr(0) = x -> wt/1
                  wt(0) = wu -> final_term
                  wu(0)
                  x(0)
                  wt/1(0)
                Pool :
                  0 |-->
                     warr(0) = x -> wt/1
                     wt/1(0)
                     x(0)
                     wt(0) = wu -> final_term
                     wu(0)
                     final_term(0)
              }
              (
                ?wt = ?warr
                ∧ (∃(?int = int). ?int = ?wt/1 ∧ ?int = ?x)
                ∧ decode ?x
              )
            in decode_scheme ?scheme_final_scheme
        ∧ ∃?y ?wt/2 (?warr/1 = ?y -> ?wt/2).
          ?wu = ?warr/1 ∧ ?wt/2 = ?y ∧ decode ?y
  -> ∃?warr.
      ∃?wt/1.
        ∃?x.
          ∃?wt.
            ∃?wu.
              let ?scheme_final_scheme : ?final_term =
                hole
                {
                  Env :
                    warr |--> wt
                    x |--> wu
                    wt/1 |--> final_term
                    final_term(0)
                    wt(0) = wu -> final_term
                    wu(0)
                  Pool :
                    0 |-->
                       warr |--> wt
                       wt/1 |--> final_term
                       x |--> wu
                       wt(0) = wu -> final_term
                       wu(0)
                       final_term(0)
                }
                (?wt = ?warr)
              in decode_scheme ?scheme_final_scheme
          ∧ ∃?y ?wt/2 (?warr/1 = ?y -> ?wt/2).
            ?wu = ?warr/1 ∧ ?wt/2 = ?y ∧ decode ?y
    ∧ decode ?x
    ∧ ∃(?int = int). ?int = ?wt/1 ∧ ?int = ?x
  -> ⊤
    ∧ ∃?warr.
      ∃?wt/1.
        ∃?x.
          ∃?wt.
            ∃?wu.
              let ?scheme_final_scheme : ?final_term =
                hole
                {
                  Env :
                    warr |--> wt
                    x |--> wu
                    wt/1 |--> final_term
                    final_term(0)
                    int(0) = int
                    wt(0) = wu -> final_term
                    wu(0)
                  Pool :
                    0 |-->
                       warr |--> wt
                       wt/1 |--> final_term
                       x |--> wu
                       int(0) = int
                       wt(0) = wu -> final_term
                       wu(0)
                       final_term(0)
                }
                (?int = ?wt/1 ∧ ?int = ?x)
              in decode_scheme ?scheme_final_scheme
          ∧ ∃?y ?wt/2 (?warr/1 = ?y -> ?wt/2).
            ?wu = ?warr/1 ∧ ?wt/2 = ?y ∧ decode ?y
    ∧ decode ?x
  -> ∃?int.
      ⊤
      ∧ ∃?warr.
        ∃?wt/1.
          ∃?x.
            ∃?wt.
              ∃?wu.
                let ?scheme_final_scheme : ?final_term =
                  hole
                  {
                    Env :
                      final_term |--> int
                      warr |--> wt
                      x |--> wu
                      wt/1 |--> int
                      int(0) = int
                      wt(0) = wu -> int
                      wu(0)
                    Pool :
                      0 |-->
                         warr |--> wt
                         wt/1 |--> int
                         x |--> wu
                         final_term |--> int
                         int(0) = int
                         wt(0) = wu -> int
                         wu(0)
                  }
                  (?int = ?wt/1)
                in decode_scheme ?scheme_final_scheme
            ∧ ∃?y ?wt/2 (?warr/1 = ?y -> ?wt/2).
              ?wu = ?warr/1 ∧ ?wt/2 = ?y ∧ decode ?y
      ∧ decode ?x
    ∧ ?int = ?x
  -> ⊤
    ∧ ∃?int.
      ⊤
      ∧ ∃?warr.
        ∃?wt/1.
          ∃?x.
            ∃?wt.
              ∃?wu.
                let ?scheme_final_scheme : ?final_term =
                  hole
                  {
                    Env :
                      final_term |--> int
                      warr |--> wt
                      wu |--> int
                      x |--> int
                      wt/1 |--> int
                      int(0) = int
                      wt(0) = int -> int
                    Pool :
                      0 |-->
                         warr |--> wt
                         wt/1 |--> int
                         x |--> int
                         wu |--> int
                         final_term |--> int
                         int(0) = int
                         wt(0) = int -> int
                  }
                  (?int = ?x)
                in decode_scheme ?scheme_final_scheme
            ∧ ∃?y ?wt/2 (?warr/1 = ?y -> ?wt/2).
              ?wu = ?warr/1 ∧ ?wt/2 = ?y ∧ decode ?y
      ∧ decode ?x
  -> ⊤
    ∧ ∃?wt.
      ∃?wu.
        let ?scheme_final_scheme : ?final_term =
          hole
          {
            Env :
              final_term |--> int
              warr |--> wt
              wu |--> int
              x |--> int
              wt/1 |--> int
              int(0) = int
              wt(0) = int -> int
              y(0)
            Pool :
              0 |-->
                 warr |--> wt
                 wt/1 |--> int
                 x |--> int
                 wu |--> int
                 final_term |--> int
                 y(0)
                 int(0) = int
                 wt(0) = int -> int
          }
          (∃?wt/2 (?warr/1 = ?y -> ?wt/2). ?wu = ?warr/1 ∧ ?wt/2 = ?y ∧ decode ?y)
        in decode_scheme ?scheme_final_scheme
  -> ∃?y.
      ⊤
      ∧ ∃?wt.
        ∃?wu.
          let ?scheme_final_scheme : ?final_term =
            hole
            {
              Env :
                final_term |--> int
                warr |--> wt
                wu |--> int
                x |--> int
                wt/1 |--> int
                int(0) = int
                wt(0) = int -> int
                y(0)
                wt/2(0)
              Pool :
                0 |-->
                   warr |--> wt
                   wt/1 |--> int
                   x |--> int
                   wu |--> int
                   final_term |--> int
                   wt/2(0)
                   y(0)
                   int(0) = int
                   wt(0) = int -> int
            }
            (∃(?warr/1 = ?y -> ?wt/2). ?wu = ?warr/1 ∧ ?wt/2 = ?y ∧ decode ?y)
          in decode_scheme ?scheme_final_scheme
  -> ∃?wt/2.
      ∃?y.
        ⊤
        ∧ ∃?wt.
          ∃?wu.
            let ?scheme_final_scheme : ?final_term =
              hole
              {
                Env :
                  final_term |--> int
                  warr |--> wt
                  wu |--> int
                  x |--> int
                  wt/1 |--> int
                  int(0) = int
                  wt(0) = int -> int
                  y(0)
                  warr/1(0) = y -> wt/2
                  wt/2(0)
                Pool :
                  0 |-->
                     warr |--> wt
                     wt/1 |--> int
                     x |--> int
                     wu |--> int
                     final_term |--> int
                     warr/1(0) = y -> wt/2
                     wt/2(0)
                     y(0)
                     int(0) = int
                     wt(0) = int -> int
              }
              (?wu = ?warr/1 ∧ ?wt/2 = ?y ∧ decode ?y)
            in decode_scheme ?scheme_final_scheme
  
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
    let ?scheme_final_scheme : ?final_term =
      ∃?x ?wt (?warr = ?x -> ?wt).
        ?final_term = ?warr
        ∧ (∃?wu (?wt/1 = ?wu -> ?wt). ?wt/1 = ?x ∧ ?wu = ?x)
        ∧ decode ?x
    in decode_scheme ?scheme_final_scheme
  
  Constraint solving log:
  -> hole {}
    (
      let ?scheme_final_scheme : ?final_term =
        ∃?x ?wt (?warr = ?x -> ?wt).
          ?final_term = ?warr
          ∧ (∃?wu (?wt/1 = ?wu -> ?wt). ?wt/1 = ?x ∧ ?wu = ?x)
          ∧ decode ?x
      in decode_scheme ?scheme_final_scheme
    )
  -> hole
    {
      Env :
        final_term(0)
      Pool :
        0 |-->
           final_term(0)
    }
    (
      let ?scheme_final_scheme : ?final_term =
        ∃?x ?wt (?warr = ?x -> ?wt).
          ?final_term = ?warr
          ∧ (∃?wu (?wt/1 = ?wu -> ?wt). ?wt/1 = ?x ∧ ?wu = ?x)
          ∧ decode ?x
      in decode_scheme ?scheme_final_scheme
    )
  -> let ?scheme_final_scheme : ?final_term =
      hole
      {
        Env :
          final_term(0)
          x(0)
        Pool :
          0 |-->
             x(0)
             final_term(0)
      }
      (
        ∃?wt (?warr = ?x -> ?wt).
          ?final_term = ?warr
          ∧ (∃?wu (?wt/1 = ?wu -> ?wt). ?wt/1 = ?x ∧ ?wu = ?x)
          ∧ decode ?x
      )
    in decode_scheme ?scheme_final_scheme
  -> ∃?x.
      let ?scheme_final_scheme : ?final_term =
        hole
        {
          Env :
            final_term(0)
            wt(0)
            x(0)
          Pool :
            0 |-->
               wt(0)
               x(0)
               final_term(0)
        }
        (
          ∃(?warr = ?x -> ?wt).
            ?final_term = ?warr
            ∧ (∃?wu (?wt/1 = ?wu -> ?wt). ?wt/1 = ?x ∧ ?wu = ?x)
            ∧ decode ?x
        )
      in decode_scheme ?scheme_final_scheme
  -> ∃?wt.
      ∃?x.
        let ?scheme_final_scheme : ?final_term =
          hole
          {
            Env :
              final_term(0)
              warr(0) = x -> wt
              wt(0)
              x(0)
            Pool :
              0 |-->
                 warr(0) = x -> wt
                 wt(0)
                 x(0)
                 final_term(0)
          }
          (
            ?final_term = ?warr
            ∧ (∃?wu (?wt/1 = ?wu -> ?wt). ?wt/1 = ?x ∧ ?wu = ?x)
            ∧ decode ?x
          )
        in decode_scheme ?scheme_final_scheme
  -> ∃?warr.
      ∃?wt.
        ∃?x.
          let ?scheme_final_scheme : ?final_term =
            hole
            {
              Env :
                warr |--> final_term
                final_term(0) = x -> wt
                wt(0)
                x(0)
              Pool :
                0 |-->
                   warr |--> final_term
                   wt(0)
                   x(0)
                   final_term(0) = x -> wt
            }
            (?final_term = ?warr)
          in decode_scheme ?scheme_final_scheme
    ∧ decode ?x
    ∧ ∃?wu (?wt/1 = ?wu -> ?wt). ?wt/1 = ?x ∧ ?wu = ?x
  -> ⊤
    ∧ ∃?warr.
      ∃?wt.
        ∃?x.
          let ?scheme_final_scheme : ?final_term =
            hole
            {
              Env :
                warr |--> final_term
                final_term(0) = x -> wt
                wt(0)
                wu(0)
                x(0)
              Pool :
                0 |-->
                   warr |--> final_term
                   wu(0)
                   wt(0)
                   x(0)
                   final_term(0) = x -> wt
            }
            (∃(?wt/1 = ?wu -> ?wt). ?wt/1 = ?x ∧ ?wu = ?x)
          in decode_scheme ?scheme_final_scheme
    ∧ decode ?x
  -> ∃?wu.
      ⊤
      ∧ ∃?warr.
        ∃?wt.
          ∃?x.
            let ?scheme_final_scheme : ?final_term =
              hole
              {
                Env :
                  warr |--> final_term
                  final_term(0) = x -> wt
                  wt(0)
                  wu(0)
                  x(0)
                  wt/1(0) = wu -> wt
                Pool :
                  0 |-->
                     warr |--> final_term
                     wt/1(0) = wu -> wt
                     wu(0)
                     wt(0)
                     x(0)
                     final_term(0) = x -> wt
              }
              (?wt/1 = ?x ∧ ?wu = ?x)
            in decode_scheme ?scheme_final_scheme
      ∧ decode ?x
  -> ∃?wt/1.
      ∃?wu.
        ⊤
        ∧ ∃?warr.
          ∃?wt.
            ∃?x.
              let ?scheme_final_scheme : ?final_term =
                hole
                {
                  Env :
                    warr |--> final_term
                    x |--> wt/1
                    final_term(0) = wt/1 -> wt
                    wt(0)
                    wu(0)
                    wt/1(0) = wu -> wt
                  Pool :
                    0 |-->
                       warr |--> final_term
                       x |--> wt/1
                       wt/1(0) = wu -> wt
                       wu(0)
                       wt(0)
                       final_term(0) = wt/1 -> wt
                }
                (?wt/1 = ?x)
              in decode_scheme ?scheme_final_scheme
        ∧ decode ?x
    ∧ ?wu = ?x
  
  Error:
    cycle on constraint variable ?wu
  






## Erroneous programs

  $ minihell $FLAGS --log-solver error_poly_def.test
  Fatal error: exception Invalid_argument("Constraint variable 'id' is unbound at this point")
  [2]




## Generator tests

This gives example outputs for my implementation. It is completely
fine if your own implementation produces different (sensible) results.

There are not many programs with size 3, 4 and 5.

  $ minigen --exhaustive --types --size 2 --count 100
  lambda (x/1 : α). x/1
  
  Inferred type : ∀α. α -> α


  $ minigen --exhaustive --types --size 3 --count 100
  lambda (v : α). lambda (w : β). v
  
  Inferred type : ∀α. ∀β. α -> β -> α
  
  
  
  lambda (v : δ). lambda (w : γ). w
  
  Inferred type : ∀δ. ∀γ. δ -> γ -> γ


  $ minigen --exhaustive --types --size 4 --count 100
  lambda (v/2 : α). lambda (y/5 : γ). lambda (z/4 : β). v/2
  
  Inferred type : ∀α. ∀γ. ∀β. α -> γ -> β -> α
  
  
  
  lambda (v/2 : β/1). lambda (y/5 : α/1). lambda (z/4 : δ). z/4
  
  Inferred type : ∀β/1. ∀α/1. ∀δ. β/1 -> α/1 -> δ -> δ
  
  
  
  lambda (v/2 : α/2). lambda (y/5 : γ/1). lambda (z/4 : δ/1). y/5
  
  Inferred type : ∀α/2. ∀γ/1. ∀δ/1. α/2 -> γ/1 -> δ/1 -> γ/1
  
  
  
  lambda (v/2 : β/2). let (v/5 : β/2) = v/2 in v/2
  
  Inferred type : ∀β/2. β/2 -> β/2
  
  
  
  lambda (v/2 : γ/2). let (v/5 : γ/2) = v/2 in v/5
  
  Inferred type : ∀γ/2. γ/2 -> γ/2
  
  
  
  lambda (v/2 : δ/2). (v/2, v/2)
  
  Inferred type : ∀δ/2. δ/2 -> {δ/2 * δ/2}
  
  
  
  lambda
  (v/2 : {α/3 * β/3}). let ((u/8 : α/3), (v/8 : β/3)) = v/2 in v/2
  
  Inferred type : ∀α/3. ∀β/3. {α/3 * β/3} -> {α/3 * β/3}
  
  
  
  lambda
  (v/2 : {γ/3 * δ/3}). let ((u/8 : γ/3), (v/8 : δ/3)) = v/2 in u/8
  
  Inferred type : ∀γ/3. ∀δ/3. {γ/3 * δ/3} -> γ/3
  
  
  
  lambda
  (v/2 : {β/4 * α/4}). let ((u/8 : β/4), (v/8 : α/4)) = v/2 in v/8
  
  Inferred type : ∀β/4. ∀α/4. {β/4 * α/4} -> α/4
  
  
  
  let Λδ/4. (y/b : δ/4 -> δ/4) = lambda (x/c : δ/4). x/c in y/b[γ/4]
  
  Inferred type : ∀γ/4. γ/4 -> γ/4


An example of random sampling output at higher size.

  $ minigen --seed 42 --types --size 6 --count 10
  (
    lambda (y/a : β/1). lambda (x/d : α/1). x/d,
    lambda (w/3 : γ/1). w/3
  )
  
  Inferred type : ∀β/1. ∀α/1. ∀γ/1. {β/1 -> α/1 -> α/1
  * γ/1 -> γ/1}
  
  
  
  (
    lambda (y/a : δ/1). lambda (x/d : α/2). y/a,
    lambda (w/3 : β/2). w/3
  )
  
  Inferred type : ∀δ/1. ∀α/2. ∀β/2. {δ/1 -> α/2 -> δ/1
  * β/2 -> β/2}
  
  
  
  (
    lambda (y/a : γ/2). y/a,
    lambda (w/3 : δ/2). lambda (u/14 : α/3). w/3
  )
  
  Inferred type : ∀γ/2. ∀δ/2. ∀α/3. {γ/2 -> γ/2
  * δ/2 -> α/3 -> δ/2}
  
  
  
  (
    lambda (y/a : β/3). y/a,
    lambda (w/3 : δ/3). lambda (u/14 : γ/3). u/14
  )
  
  Inferred type : ∀β/3. ∀δ/3. ∀γ/3. {β/3 -> β/3
  * δ/3 -> γ/3 -> γ/3}
  
  
  
  lambda
  (z/1d : α/4).
    let Λδ/4. (u/1d : δ/4 -> α/4) = lambda (y/1f : δ/4). z/1d in
      lambda (v/1e : γ/4). u/1d[β/4]
  
  Inferred type : ∀α/4. ∀γ/4. ∀β/4. α/4 -> γ/4 -> β/4 -> α/4
  
  
  
  lambda
  (z/1d : α/5).
    let Λγ/5. (u/1d : γ/5 -> α/5) = lambda (y/1f : γ/5). z/1d in
      lambda (v/1e : β/5). z/1d
  
  Inferred type : ∀α/5. ∀β/5. α/5 -> β/5 -> α/5
  
  
  
  lambda
  (z/1d : α/6).
    let Λβ/6. (u/1d : β/6 -> α/6) = lambda (y/1f : β/6). z/1d in
      lambda (v/1e : δ/5). v/1e
  
  Inferred type : ∀α/6. ∀δ/5. α/6 -> δ/5 -> δ/5
  
  
  
  lambda
  (z/1d : δ/6).
    let Λα/7. (u/1d : α/7 -> α/7) = lambda (y/1f : α/7). y/1f in
      lambda (v/1e : γ/6). v/1e
  
  Inferred type : ∀δ/6. ∀γ/6. δ/6 -> γ/6 -> γ/6
  
  
  
  lambda
  (z/1d : δ/7).
    let Λα/8. (u/1d : α/8 -> α/8) = lambda (y/1f : α/8). y/1f in
      lambda (v/1e : γ/7). u/1d[β/7]
  
  Inferred type : ∀δ/7. ∀γ/7. ∀β/7. δ/7 -> γ/7 -> β/7 -> β/7
  
  
  
  lambda
  (z/1d : β/8).
    let Λδ/8. (u/1d : δ/8 -> δ/8) = lambda (y/1f : δ/8). y/1f in
      lambda (v/1e : γ/8). z/1d
  
  Inferred type : ∀β/8. ∀γ/8. β/8 -> γ/8 -> β/8

  $ dune exec -- minigen --exhaustive --types --size 10 --count 1
  (lambda (v : α -> γ -> β -> α). v)
    ((lambda (u/1 : α -> γ -> β -> α). u/1)
      (lambda (z/4 : α). lambda (w/5 : γ). lambda (x/7 : β). z/4))
  
  Inferred type : ∀α. ∀γ. ∀β. α -> γ -> β -> α
