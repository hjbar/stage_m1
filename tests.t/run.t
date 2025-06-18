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
  




  $ minihell $FLAGS id_poly2.test
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
(Var ...)` at type `STLC.ty`.

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
  




  $ minihell $FLAGS let_easy.test
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

  $ minihell $FLAGS poly_easy.test
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
  




  $ minihell $FLAGS poly1.test
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
  





  $ minihell $FLAGS poly2.test
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
  ->hole {}
    (
      let ?scheme_final_scheme : ?final_term =
        ∃?x ?wt (?warr = ?x -> ?wt).
          ?final_term = ?warr
          ∧ (∃(?int = int). ?int = ?wt ∧ ?int = ?x)
          ∧ decode ?x
      in decode_scheme ?scheme_final_scheme
    )
  ->hole
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
  ->let ?scheme_final_scheme : ?final_term =
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
  ->∃?x.
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
  ->∃?wt.
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
  ->∃?warr.
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
  ->⊤
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
  ->∃?int.
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
  ->⊤
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
  <-let ?scheme_final_scheme : ?final_term =
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
  ->hole {}
    (
      let ?scheme_final_scheme : ?final_term =
        ∃?wu (?wt = ?wu -> ?final_term).
          (∃?x ?wt/1 (?warr = ?x -> ?wt/1).
            ?wt = ?warr ∧ (∃(?int = int). ?int = ?wt/1 ∧ ?int = ?x) ∧ decode ?x)
          ∧ (∃?y ?wt/2 (?warr/1 = ?y -> ?wt/2).
            ?wu = ?warr/1 ∧ ?wt/2 = ?y ∧ decode ?y)
      in decode_scheme ?scheme_final_scheme
    )
  ->hole
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
  ->let ?scheme_final_scheme : ?final_term =
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
  ->∃?wu.
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
  ->∃?wt.
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
  ->∃?x.
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
  ->∃?wt/1.
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
  ->∃?warr.
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
  ->⊤
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
  ->∃?int.
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
  ->⊤
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
  ->⊤
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
  ->∃?y.
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
  ->∃?wt/2.
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
  ->hole {}
    (
      let ?scheme_final_scheme : ?final_term =
        ∃?x ?wt (?warr = ?x -> ?wt).
          ?final_term = ?warr
          ∧ (∃?wu (?wt/1 = ?wu -> ?wt). ?wt/1 = ?x ∧ ?wu = ?x)
          ∧ decode ?x
      in decode_scheme ?scheme_final_scheme
    )
  ->hole
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
  ->let ?scheme_final_scheme : ?final_term =
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
  ->∃?x.
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
  ->∃?wt.
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
  ->∃?warr.
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
  ->⊤
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
  ->∃?wu.
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
  ->∃?wt/1.
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
  lambda (v : α). v
  
  Inferred type : ∀α. α -> α


  $ minigen --exhaustive --types --size 3 --count 100
  lambda (x/4 : β/1). lambda (w/4 : α/1). w/4
  
  Inferred type : ∀β/1. ∀α/1. β/1 -> α/1 -> α/1
  
  
  
  lambda (x/4 : γ/1). lambda (w/4 : δ/1). x/4
  
  Inferred type : ∀γ/1. ∀δ/1. γ/1 -> δ/1 -> γ/1


  $ minigen --exhaustive --types --size 4 --count 100
  lambda (v/14 : α/7). lambda (u/19 : γ/7). lambda (z/1a : β/7). v/14
  
  Inferred type : ∀α/7. ∀γ/7. ∀β/7. α/7 -> γ/7 -> β/7 -> α/7
  
  
  
  lambda (v/14 : β/8). lambda (u/19 : δ/7). lambda (z/1a : α/8). u/19
  
  Inferred type : ∀β/8. ∀δ/7. ∀α/8. β/8 -> δ/7 -> α/8 -> δ/7
  
  
  
  lambda (v/14 : α/9). lambda (u/19 : δ/8). lambda (z/1a : γ/8). z/1a
  
  Inferred type : ∀α/9. ∀δ/8. ∀γ/8. α/9 -> δ/8 -> γ/8 -> γ/8
  
  
  
  lambda (v/14 : β/a). let (v/1d : β/a) = v/14 in v/14
  
  Inferred type : ∀β/a. β/a -> β/a
  
  
  
  lambda (v/14 : γ/a). let (v/1d : γ/a) = v/14 in v/1d
  
  Inferred type : ∀γ/a. γ/a -> γ/a
  
  
  
  lambda (v/14 : δ/b). (v/14, v/14)
  
  Inferred type : ∀δ/b. δ/b -> {δ/b * δ/b}
  
  
  
  lambda
  (v/14 : {α/d * β/d}).
    let ((y/27 : α/d), (z/27 : β/d)) = v/14 in v/14
  
  Inferred type : ∀α/d. ∀β/d. {α/d * β/d} -> {α/d * β/d}
  
  
  
  lambda
  (v/14 : {γ/d * δ/d}).
    let ((y/27 : γ/d), (z/27 : δ/d)) = v/14 in y/27
  
  Inferred type : ∀γ/d. ∀δ/d. {γ/d * δ/d} -> γ/d
  
  
  
  lambda
  (v/14 : {β/e * α/e}).
    let ((y/27 : β/e), (z/27 : α/e)) = v/14 in z/27
  
  Inferred type : ∀β/e. ∀α/e. {β/e * α/e} -> α/e
  
  
  
  let Λδ/11. (u/2b : δ/11 -> δ/11) = lambda (v/2f : δ/11). v/2f in
    u/2b[γ/11]
  
  Inferred type : ∀γ/11. γ/11 -> γ/11


An example of random sampling output at higher size.

  $ minigen --seed 42 --types --size 6 --count 10
  lambda
  (z/7 : δ/128).
    (lambda (v/364 : α/129). z/7, lambda (w/364 : β/129). w/364)
  
  Inferred type : ∀δ/128. ∀α/129. ∀β/129. δ/128
  ->
  {α/129 -> δ/128 * β/129 -> β/129}
  
  
  
  lambda
  (z/7 : δ/135).
    (lambda (u/64 : δ/135). lambda (u/392 : γ/135). u/392) z/7
  
  Inferred type : ∀δ/135. ∀γ/135. δ/135 -> γ/135 -> γ/135
  
  
  
  let Λβ/187. (w/2 : β/187 -> β/187) = lambda (z/e : β/187). z/e in
    w/2[α/187 -> α/187] w/2[α/187]
  
  Inferred type : ∀α/187. α/187 -> α/187
  
  
  
  lambda
  (z/7 : {γ/1b0 * δ/1b0}).
    let ((w/551 : γ/1b0), (x/552 : δ/1b0)) =
      let ((y/552 : γ/1b0), (z/552 : δ/1b0)) = z/7 in z/7
    in
      w/551
  
  Inferred type : ∀γ/1b0. ∀δ/1b0. {γ/1b0 * δ/1b0} -> γ/1b0
  
  
  
  lambda
  (z/7 : {δ/227 * α/228}).
    let ((z/6ed : δ/227), (u/6ed : α/228)) = z/7 in
      let (v/6ed : {δ/227 * α/228}) = z/7 in z/6ed
  
  Inferred type : ∀δ/227. ∀α/228. {δ/227 * α/228} -> δ/227
  
  
  
  (
    lambda (x/788 : β/259). lambda (y/788 : α/259). y/788,
    lambda (z/788 : γ/259). z/788
  )
  
  Inferred type : ∀β/259. ∀α/259. ∀γ/259. {β/259
  ->
  α/259 -> α/259
  * γ/259 -> γ/259}
  
  
  
  lambda
  (z/7 : δ/259).
    lambda (z/18 : α/25a). lambda (w/21 : β/25a). (z/7, z/18)
  
  Inferred type : ∀δ/259. ∀α/25a. ∀β/25a. δ/259
  ->
  α/25a -> β/25a -> {δ/259 * α/25a}
  
  
  
  lambda
  (z/7 : {β/35d * γ/35d}).
    let (z/f : {β/35d * γ/35d}) =
      let ((x/a61 : β/35d), (y/a61 : γ/35d)) = z/7 in z/7
    in
      z/f
  
  Inferred type : ∀β/35d. ∀γ/35d. {β/35d * γ/35d}
  ->
  {β/35d * γ/35d}
  
  
  
  lambda
  (z/7 : {α/515 * β/515}).
    (z/7, let ((v/fa0 : α/515), (w/fa0 : β/515)) = z/7 in z/7)
  
  Inferred type : ∀α/515. ∀β/515. {α/515 * β/515}
  ->
  {{α/515 * β/515} * {α/515 * β/515}}
  
  
  
  let Λβ/548. Λα/548. (w/2 : β/548 -> α/548 -> α/548) =
    lambda (z/e : β/548). lambda (w/24 : α/548). w/24
  in
    lambda (y/a5 : δ/547). y/a5
  
  Inferred type : ∀δ/547. δ/547 -> δ/547

  $ dune exec -- minigen --exhaustive --types --size 10 --count 1
  (lambda
  (
    z/4553
    :
      (γ/1d49 -> α/1d4a -> δ/1d49 -> γ/1d49)
      ->
      γ/1d49 -> α/1d4a -> δ/1d49 -> γ/1d49
  ). z/4553)
    (lambda (w/4788 : γ/1d49 -> α/1d4a -> δ/1d49 -> γ/1d49). w/4788)
    (lambda
    (v/479d : γ/1d49).
      lambda (u/47a2 : α/1d4a). lambda (z/47a3 : δ/1d49). v/479d)
  
  Inferred type : ∀γ/1d49. ∀α/1d4a. ∀δ/1d49. γ/1d49
  ->
  α/1d4a -> δ/1d49 -> γ/1d49
