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
  Input term: lambda x. x
  Generated constraint:
    let ?final_scheme : ?final_term =
      ∃?x ?wt (?warr = ?x -> ?wt). ?final_term = ?warr ∧ ?wt = ?x ∧ decode ?x
    in decode_scheme ?final_scheme
  Inferred type: ∀α. α -> α
  Elaborated term: lambda (x : α). x




  $ minihell $FLAGS id_poly_with_let.test
  Input term: lambda y. let id = lambda x. x in id y
  Generated constraint:
    let ?final_scheme : ?final_term =
      ∃?y ?wt (?warr = ?y -> ?wt).
        ?final_term = ?warr
        ∧ (let ?scheme_id : ?id =
          ∃?x ?wt/2 (?warr/1 = ?x -> ?wt/2).
            ?id = ?warr/1 ∧ ?wt/2 = ?x ∧ decode ?x
        in
          (∃?wu (?wt/1 = ?wu -> ?wt). ?scheme_id ≤ ?wt/1 ∧ ?wu = ?y)
          ∧ decode_scheme ?scheme_id)
        ∧ decode ?y
    in decode_scheme ?final_scheme
  Inferred type: ∀α. α -> α
  Elaborated term:
    lambda
    (y : α).
      let (id : ∀β. β -> β) = Λβ. lambda (x : β). x in id[α] y




`id_int` is the monomorphic identity on the type `int`. Note
that we have not implemented support for a built-in `int`
type, this is just an abstract/rigid type variable: `Constr
(Var ...)` at type `F.ty`.

  $ minihell $FLAGS id_int.test
  Input term: lambda x. (x : int)
  Generated constraint:
    let ?final_scheme : ?final_term =
      ∃?x ?wt (?warr = ?x -> ?wt).
        ?final_term = ?warr
        ∧ (∃(?int = int). ?int = ?wt ∧ ?int = ?x)
        ∧ decode ?x
    in decode_scheme ?final_scheme
  Inferred type: int -> int
  Elaborated term: lambda (x : int). (x : int)




  $ minihell $FLAGS let_poly.test
  Input term: let x = lambda y. y in x
  Generated constraint:
    let ?final_scheme : ?final_term =
      let ?scheme_x : ?x =
        ∃?y ?wt (?warr = ?y -> ?wt). ?x = ?warr ∧ ?wt = ?y ∧ decode ?y
      in ?scheme_x ≤ ?final_term ∧ decode_scheme ?scheme_x
    in decode_scheme ?final_scheme
  Inferred type: ∀α. α -> α
  Elaborated term:
    let (x : ∀β. β -> β) = Λβ. lambda (y : β). y in x[α]






## Examples with products

  $ minihell $FLAGS curry.test
  Input term: lambda f. lambda x. lambda y. f (x, y)
  Generated constraint:
    let ?final_scheme : ?final_term =
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
    in decode_scheme ?final_scheme
  Inferred type: ∀β. ∀γ. ∀α. ({γ * β} -> α) -> γ -> β -> α
  Elaborated term:
    lambda (f : {γ * β} -> α). lambda (x : γ). lambda (y : β). f (x, y)




  $ minihell $FLAGS uncurry.test
  Input term: lambda f. lambda p. let (x, y) = p in f x y
  Generated constraint:
    let ?final_scheme : ?final_term =
      ∃?f ?wt (?warr = ?f -> ?wt).
        ?final_term = ?warr
        ∧ (∃?p ?wt/1 (?warr/1 = ?p -> ?wt/1).
          ?wt = ?warr/1
          ∧ (let (?scheme_x : ?x, ?scheme_y : ?y) =
            ∃(?wt/2 = {?x * ?y}). ?wt/2 = ?p
          in
            (∃?wu (?wt/3 = ?wu -> ?wt/1).
              (∃?wu/1 (?wt/4 = ?wu/1 -> ?wt/3). ?wt/4 = ?f ∧ ?scheme_x ≤ ?wu/1)
              ∧ ?scheme_y ≤ ?wu)
            ∧ decode_scheme ?scheme_y
            ∧ decode_scheme ?scheme_x)
          ∧ decode ?p)
        ∧ decode ?f
    in decode_scheme ?final_scheme
  Inferred type: ∀β. ∀γ. ∀α. (β -> γ -> α) -> {β * γ} -> α
  Elaborated term:
    lambda
    (f : β -> γ -> α).
      lambda (p : {β * γ}). let ((x : β), (y : γ)) = p in f x y






## Polymorhpism

  $ minihell $FLAGS poly_easy_use.test
  Input term: lambda a. let id = lambda x. x in let r = id a in r
  Generated constraint:
    let ?final_scheme : ?final_term =
      ∃?a ?wt (?warr = ?a -> ?wt).
        ?final_term = ?warr
        ∧ (let ?scheme_id : ?id =
          ∃?x ?wt/2 (?warr/1 = ?x -> ?wt/2).
            ?id = ?warr/1 ∧ ?wt/2 = ?x ∧ decode ?x
        in
          (let ?scheme_r : ?r =
            ∃?wu (?wt/1 = ?wu -> ?r). ?scheme_id ≤ ?wt/1 ∧ ?wu = ?a
          in ?scheme_r ≤ ?wt ∧ decode_scheme ?scheme_r)
          ∧ decode_scheme ?scheme_id)
        ∧ decode ?a
    in decode_scheme ?final_scheme
  Inferred type: ∀α. α -> α
  Elaborated term:
    lambda
    (a : α).
      let (id : ∀β. β -> β) = Λβ. lambda (x : β). x in
        let (r : α) = id[α] a in r




  $ minihell $FLAGS poly_use.test
  Input term:
    let id = lambda x. x in
      lambda a. lambda b. let l = id a in let r = id b in (l, r)
  Generated constraint:
    let ?final_scheme : ?final_term =
      let ?scheme_id : ?id =
        ∃?x ?wt/4 (?warr/2 = ?x -> ?wt/4).
          ?id = ?warr/2 ∧ ?wt/4 = ?x ∧ decode ?x
      in
        (∃?a ?wt (?warr = ?a -> ?wt).
          ?final_term = ?warr
          ∧ (∃?b ?wt/1 (?warr/1 = ?b -> ?wt/1).
            ?wt = ?warr/1
            ∧ (let ?scheme_l : ?l =
              ∃?wu/1 (?wt/3 = ?wu/1 -> ?l). ?scheme_id ≤ ?wt/3 ∧ ?wu/1 = ?a
            in
              (let ?scheme_r : ?r =
                ∃?wu (?wt/2 = ?wu -> ?r). ?scheme_id ≤ ?wt/2 ∧ ?wu = ?b
              in
                (∃?w1.
                  ?scheme_l ≤ ?w1
                  ∧ (∃?w2.
                    ?scheme_r ≤ ?w2 ∧ (∃(?wprod = {?w1 * ?w2}). ?wt/1 = ?wprod)))
                ∧ decode_scheme ?scheme_r)
              ∧ decode_scheme ?scheme_l)
            ∧ decode ?b)
          ∧ decode ?a)
        ∧ decode_scheme ?scheme_id
    in decode_scheme ?final_scheme
  Inferred type: ∀β. ∀α. α -> β -> {α * β}
  Elaborated term:
    let (id : ∀γ. γ -> γ) = Λγ. lambda (x : γ). x in
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
    let ?final_scheme : ?final_term =
      let ?scheme_id : ?id =
        ∃?x ?wt/4 (?warr/2 = ?x -> ?wt/4).
          ?id = ?warr/2 ∧ ?wt/4 = ?x ∧ decode ?x
      in
        (∃?a ?wt (?warr = ?a -> ?wt).
          ?final_term = ?warr
          ∧ (∃?b ?wt/1 (?warr/1 = ?b -> ?wt/1).
            ?wt = ?warr/1
            ∧ (let ?scheme_l : ?l =
              ∃?wu/1 (?wt/3 = ?wu/1 -> ?l). ?scheme_id ≤ ?wt/3 ∧ ?wu/1 = ?a
            in
              (let ?scheme_r : ?r =
                ∃?wu (?wt/2 = ?wu -> ?r). ?scheme_id ≤ ?wt/2 ∧ ?wu = ?b
              in
                (∃?w1.
                  (∃(?int = int). ?int = ?w1 ∧ ?scheme_l ≤ ?int)
                  ∧ (∃?w2.
                    (∃(?bool = bool). ?bool = ?w2 ∧ ?scheme_r ≤ ?bool)
                    ∧ (∃(?wprod = {?w1 * ?w2}). ?wt/1 = ?wprod)))
                ∧ decode_scheme ?scheme_r)
              ∧ decode_scheme ?scheme_l)
            ∧ decode ?b)
          ∧ decode ?a)
        ∧ decode_scheme ?scheme_id
    in decode_scheme ?final_scheme
  Inferred type: int -> bool -> {int * bool}
  Elaborated term:
    let (id : ∀α. α -> α) = Λα. lambda (x : α). x in
      lambda
      (a : int).
        lambda
        (b : bool).
          let (l : int) = id[int] a in
            let (r : bool) = id[bool] b in ((l : int), (r : bool))




  $ minihell $FLAGS tuple_poly_use.test
  Input term:
    let (id1, id2) = (lambda x. x, lambda x/1. x/1) in
      lambda
      a.
        lambda
        b.
          lambda
          c.
            lambda
            d.
              let l1 = id1 a in
                let r1 = id1 b in
                  let l2 = id2 c in let r2 = id2 d in ((l1, r1), (l2, r2))
  Generated constraint:
    let ?final_scheme : ?final_term =
      let (?scheme_id1 : ?id1, ?scheme_id2 : ?id2) =
        ∃(?wt = {?id1 * ?id2}) ?w1/3.
          (∃?x ?wt/9 (?warr/4 = ?x -> ?wt/9).
            ?w1/3 = ?warr/4 ∧ ?wt/9 = ?x ∧ decode ?x)
          ∧ (∃?w2/3.
            (∃?x/1 ?wt/a (?warr/5 = ?x/1 -> ?wt/a).
              ?w2/3 = ?warr/5 ∧ ?wt/a = ?x/1 ∧ decode ?x/1)
            ∧ (∃(?wprod/3 = {?w1/3 * ?w2/3}). ?wt = ?wprod/3))
      in
        (∃?a ?wt/1 (?warr = ?a -> ?wt/1).
          ?final_term = ?warr
          ∧ (∃?b ?wt/2 (?warr/1 = ?b -> ?wt/2).
            ?wt/1 = ?warr/1
            ∧ (∃?c ?wt/3 (?warr/2 = ?c -> ?wt/3).
              ?wt/2 = ?warr/2
              ∧ (∃?d ?wt/4 (?warr/3 = ?d -> ?wt/4).
                ?wt/3 = ?warr/3
                ∧ (let ?scheme_l1 : ?l1 =
                  ∃?wu/3 (?wt/8 = ?wu/3 -> ?l1). ?scheme_id1 ≤ ?wt/8 ∧ ?wu/3 = ?a
                in
                  (let ?scheme_r1 : ?r1 =
                    ∃?wu/2 (?wt/7 = ?wu/2 -> ?r1).
                      ?scheme_id1 ≤ ?wt/7 ∧ ?wu/2 = ?b
                  in
                    (let ?scheme_l2 : ?l2 =
                      ∃?wu/1 (?wt/6 = ?wu/1 -> ?l2).
                        ?scheme_id2 ≤ ?wt/6 ∧ ?wu/1 = ?c
                    in
                      (let ?scheme_r2 : ?r2 =
                        ∃?wu (?wt/5 = ?wu -> ?r2). ?scheme_id2 ≤ ?wt/5 ∧ ?wu = ?d
                      in
                        (∃?w1.
                          (∃?w1/1.
                            ?scheme_l1 ≤ ?w1/1
                            ∧ (∃?w2.
                              ?scheme_r1 ≤ ?w2
                              ∧ (∃(?wprod = {?w1/1 * ?w2}). ?w1 = ?wprod)))
                          ∧ (∃?w2/1.
                            (∃?w1/2.
                              ?scheme_l2 ≤ ?w1/2
                              ∧ (∃?w2/2.
                                ?scheme_r2 ≤ ?w2/2
                                ∧ (∃(?wprod/1 = {?w1/2 * ?w2/2}). ?w2/1 = ?wprod/1)))
                            ∧ (∃(?wprod/2 = {?w1 * ?w2/1}). ?wt/4 = ?wprod/2)))
                        ∧ decode_scheme ?scheme_r2)
                      ∧ decode_scheme ?scheme_l2)
                    ∧ decode_scheme ?scheme_r1)
                  ∧ decode_scheme ?scheme_l1)
                ∧ decode ?d)
              ∧ decode ?c)
            ∧ decode ?b)
          ∧ decode ?a)
        ∧ decode_scheme ?scheme_id2
        ∧ decode_scheme ?scheme_id1
    in decode_scheme ?final_scheme
  Inferred type:
    ∀α/1. ∀δ. ∀β/1. ∀γ. ∀γ/1. ∀β. ∀δ/1. ∀α.
      α -> β -> γ -> δ -> {{α * β} * {γ * δ}}
  Elaborated term:
    let
    (
      (id1 : ∀β/2. ∀α/2. α/2 -> α/2),
      (id2 : ∀β/2. ∀α/2. β/2 -> β/2)
    )
    =
      (lambda (x : α/2). x, lambda (x/1 : β/2). x/1)
    in
      lambda
      (a : α).
        lambda
        (b : β).
          lambda
          (c : γ).
            lambda
            (d : δ).
              let (l1 : ∀γ/2. α) = Λγ/2. id1[γ/2, α] a in
                let (r1 : ∀δ/2. β) = Λδ/2. id1[δ/2, β] b in
                  let (l2 : ∀α/3. γ) = Λα/3. id2[γ, α/3] c in
                    let (r2 : ∀β/3. δ) = Λβ/3. id2[δ, β/3] d in
                      ((l1[δ/1], r1[γ/1]), (l2[β/1], r2[α/1]))




  $ minihell $FLAGS tuple_poly_annot_use.test
  Input term:
    let (id1, id2) = (lambda x. x, lambda x/1. x/1) in
      lambda
      a.
        lambda
        b.
          lambda
          c.
            lambda
            d.
              let l1 = id1 a in
                let r1 = id1 b in
                  let l2 = id2 c in
                    let r2 = id2 d in
                      (((l1 : int), (r1 : bool)), ((l2 : char), (r2 : string)))
  Generated constraint:
    let ?final_scheme : ?final_term =
      let (?scheme_id1 : ?id1, ?scheme_id2 : ?id2) =
        ∃(?wt = {?id1 * ?id2}) ?w1/3.
          (∃?x ?wt/9 (?warr/4 = ?x -> ?wt/9).
            ?w1/3 = ?warr/4 ∧ ?wt/9 = ?x ∧ decode ?x)
          ∧ (∃?w2/3.
            (∃?x/1 ?wt/a (?warr/5 = ?x/1 -> ?wt/a).
              ?w2/3 = ?warr/5 ∧ ?wt/a = ?x/1 ∧ decode ?x/1)
            ∧ (∃(?wprod/3 = {?w1/3 * ?w2/3}). ?wt = ?wprod/3))
      in
        (∃?a ?wt/1 (?warr = ?a -> ?wt/1).
          ?final_term = ?warr
          ∧ (∃?b ?wt/2 (?warr/1 = ?b -> ?wt/2).
            ?wt/1 = ?warr/1
            ∧ (∃?c ?wt/3 (?warr/2 = ?c -> ?wt/3).
              ?wt/2 = ?warr/2
              ∧ (∃?d ?wt/4 (?warr/3 = ?d -> ?wt/4).
                ?wt/3 = ?warr/3
                ∧ (let ?scheme_l1 : ?l1 =
                  ∃?wu/3 (?wt/8 = ?wu/3 -> ?l1). ?scheme_id1 ≤ ?wt/8 ∧ ?wu/3 = ?a
                in
                  (let ?scheme_r1 : ?r1 =
                    ∃?wu/2 (?wt/7 = ?wu/2 -> ?r1).
                      ?scheme_id1 ≤ ?wt/7 ∧ ?wu/2 = ?b
                  in
                    (let ?scheme_l2 : ?l2 =
                      ∃?wu/1 (?wt/6 = ?wu/1 -> ?l2).
                        ?scheme_id2 ≤ ?wt/6 ∧ ?wu/1 = ?c
                    in
                      (let ?scheme_r2 : ?r2 =
                        ∃?wu (?wt/5 = ?wu -> ?r2). ?scheme_id2 ≤ ?wt/5 ∧ ?wu = ?d
                      in
                        (∃?w1.
                          (∃?w1/1.
                            (∃(?int = int). ?int = ?w1/1 ∧ ?scheme_l1 ≤ ?int)
                            ∧ (∃?w2.
                              (∃(?bool = bool). ?bool = ?w2 ∧ ?scheme_r1 ≤ ?bool)
                              ∧ (∃(?wprod = {?w1/1 * ?w2}). ?w1 = ?wprod)))
                          ∧ (∃?w2/1.
                            (∃?w1/2.
                              (∃(?char = char). ?char = ?w1/2 ∧ ?scheme_l2 ≤ ?char)
                              ∧ (∃?w2/2.
                                (∃(?string = string).
                                  ?string = ?w2/2 ∧ ?scheme_r2 ≤ ?string)
                                ∧ (∃(?wprod/1 = {?w1/2 * ?w2/2}). ?w2/1 = ?wprod/1)))
                            ∧ (∃(?wprod/2 = {?w1 * ?w2/1}). ?wt/4 = ?wprod/2)))
                        ∧ decode_scheme ?scheme_r2)
                      ∧ decode_scheme ?scheme_l2)
                    ∧ decode_scheme ?scheme_r1)
                  ∧ decode_scheme ?scheme_l1)
                ∧ decode ?d)
              ∧ decode ?c)
            ∧ decode ?b)
          ∧ decode ?a)
        ∧ decode_scheme ?scheme_id2
        ∧ decode_scheme ?scheme_id1
    in decode_scheme ?final_scheme
  Inferred type:
    ∀α. ∀β. ∀γ. ∀δ.
      int -> bool -> char -> string -> {{int * bool} * {char * string}}
  Elaborated term:
    let
    (
      (id1 : ∀β/1. ∀α/1. α/1 -> α/1),
      (id2 : ∀β/1. ∀α/1. β/1 -> β/1)
    )
    =
      (lambda (x : α/1). x, lambda (x/1 : β/1). x/1)
    in
      lambda
      (a : int).
        lambda
        (b : bool).
          lambda
          (c : char).
            lambda
            (d : string).
              let (l1 : ∀γ/1. int) = Λγ/1. id1[γ/1, int] a in
                let (r1 : ∀δ/1. bool) = Λδ/1. id1[δ/1, bool] b in
                  let (l2 : ∀α/2. char) = Λα/2. id2[char, α/2] c in
                    let (r2 : ∀β/2. string) = Λβ/2. id2[string, β/2] d in
                      (
                        ((l1[δ] : int), (r1[γ] : bool)),
                        ((l2[β] : char), (r2[α] : string))
                      )




## System F

We test an example where a type variable occurs in the body
of a definition but not in the inferred type scheme.

  $ minihell $FLAGS poly_silent_var.test
  Input term: let test = lambda x. (lambda z. x) (lambda y. y) in test
  Generated constraint:
    let ?final_scheme : ?final_term =
      let ?scheme_test : ?test =
        ∃?x ?wt (?warr = ?x -> ?wt).
          ?test = ?warr
          ∧ (∃?wu (?wt/1 = ?wu -> ?wt).
            (∃?z ?wt/2 (?warr/1 = ?z -> ?wt/2).
              ?wt/1 = ?warr/1 ∧ ?wt/2 = ?x ∧ decode ?z)
            ∧ (∃?y ?wt/3 (?warr/2 = ?y -> ?wt/3).
              ?wu = ?warr/2 ∧ ?wt/3 = ?y ∧ decode ?y))
          ∧ decode ?x
      in ?scheme_test ≤ ?final_term ∧ decode_scheme ?scheme_test
    in decode_scheme ?final_scheme
  Inferred type: ∀α. ∀β. α -> α
  Elaborated term:
    let (test : ∀δ. ∀γ. γ -> γ) =
      Λδ. Λγ.
      lambda (x : γ). (lambda (z : δ -> δ). x) (lambda (y : δ). y)
    in
      test[β, α]




## Logging the constraint-solving process

You can ask `minihell` to show how the constraint evolves as
the solver progresses and accumulates more information on
the inference variables.

  $ minihell $FLAGS --log-solver id_int.test
  Input term: lambda x. (x : int)
  Generated constraint:
    let ?final_scheme : ?final_term =
      ∃?x ?wt (?warr = ?x -> ?wt).
        ?final_term = ?warr
        ∧ (∃(?int = int). ?int = ?wt ∧ ?int = ?x)
        ∧ decode ?x
    in decode_scheme ?final_scheme
  Constraint solving log:
  -- hole {}
    (
      let ?final_scheme : ?final_term =
        ∃?x ?wt (?warr = ?x -> ?wt).
          ?final_term = ?warr
          ∧ (∃(?int = int). ?int = ?wt ∧ ?int = ?x)
          ∧ decode ?x
      in decode_scheme ?final_scheme
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
      let ?final_scheme : ?final_term =
        ∃?x ?wt (?warr = ?x -> ?wt).
          ?final_term = ?warr
          ∧ (∃(?int = int). ?int = ?wt ∧ ?int = ?x)
          ∧ decode ?x
      in decode_scheme ?final_scheme
    )
  -> let ?final_scheme : ?final_term =
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
    in decode_scheme ?final_scheme
  -> let ?final_scheme : ?final_term =
      ∃?x.
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
    in decode_scheme ?final_scheme
  -> let ?final_scheme : ?final_term =
      ∃?x.
        ∃?wt.
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
    in decode_scheme ?final_scheme
  -> let ?final_scheme : ?final_term =
      ∃?x.
        ∃?wt.
          ∃?warr.
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
            ∧ ∃(?int = int). ?int = ?wt ∧ ?int = ?x
            ∧ decode ?x
    in decode_scheme ?final_scheme
  -> let ?final_scheme : ?final_term =
      ∃?x.
        ∃?wt.
          ∃?warr.
            ⊤
            ∧ hole
            {
              Env :
                warr |--> final_term
                final_term(0) = x -> wt
                int(0) = int
                wt(0)
                x(0)
              Pool :
                0 |-->
                   int(0) = int
                   warr |--> final_term
                   wt(0)
                   x(0)
                   final_term(0) = x -> wt
            }
            (?int = ?wt ∧ ?int = ?x)
            ∧ decode ?x
    in decode_scheme ?final_scheme
  -> let ?final_scheme : ?final_term =
      ∃?x.
        ∃?wt.
          ∃?warr.
            ⊤
            ∧ ∃?int.
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
                     int(0) = int
                     warr |--> final_term
                     wt |--> int
                     x(0)
                     final_term(0) = x -> int
              }
              (?int = ?wt)
              ∧ ?int = ?x
            ∧ decode ?x
    in decode_scheme ?final_scheme
  -> let ?final_scheme : ?final_term =
      ∃?x.
        ∃?wt.
          ∃?warr.
            ⊤
            ∧ ∃?int.
              ⊤
              ∧ hole
              {
                Env :
                  warr |--> final_term
                  wt |--> int
                  x |--> int
                  final_term(0) = int -> int
                  int(0) = int
                Pool :
                  0 |-->
                     int(0) = int
                     warr |--> final_term
                     wt |--> int
                     x |--> int
                     final_term(0) = int -> int
              }
              (?int = ?x)
            ∧ decode ?x
    in decode_scheme ?final_scheme
  <- let ?final_scheme : ?final_term =
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
    in decode_scheme ?final_scheme
  <- let ?final_scheme = ⊤
    in
    hole
    {
      Env :
        warr |--> final_term
        wt |--> int
        x |--> int
        final_term(G) = int -> int
        int(G) = int
  
    }
    (⊤)
  
  Inferred type: int -> int
  Elaborated term: lambda (x : int). (x : int)




 $ minihell $FLAGS --log-solver log_solver.test





## Clash types

  $ minihell $FLAGS --log-solver error_clash_types.test
  Input term: (lambda x. (x : int)) (lambda y. y)
  Generated constraint:
    let ?final_scheme : ?final_term =
      ∃?wu (?wt = ?wu -> ?final_term).
        (∃?x ?wt/1 (?warr = ?x -> ?wt/1).
          ?wt = ?warr ∧ (∃(?int = int). ?int = ?wt/1 ∧ ?int = ?x) ∧ decode ?x)
        ∧ (∃?y ?wt/2 (?warr/1 = ?y -> ?wt/2).
          ?wu = ?warr/1 ∧ ?wt/2 = ?y ∧ decode ?y)
    in decode_scheme ?final_scheme
  Constraint solving log:
  -- hole {}
    (
      let ?final_scheme : ?final_term =
        ∃?wu (?wt = ?wu -> ?final_term).
          (∃?x ?wt/1 (?warr = ?x -> ?wt/1).
            ?wt = ?warr ∧ (∃(?int = int). ?int = ?wt/1 ∧ ?int = ?x) ∧ decode ?x)
          ∧ (∃?y ?wt/2 (?warr/1 = ?y -> ?wt/2).
            ?wu = ?warr/1 ∧ ?wt/2 = ?y ∧ decode ?y)
      in decode_scheme ?final_scheme
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
      let ?final_scheme : ?final_term =
        ∃?wu (?wt = ?wu -> ?final_term).
          (∃?x ?wt/1 (?warr = ?x -> ?wt/1).
            ?wt = ?warr ∧ (∃(?int = int). ?int = ?wt/1 ∧ ?int = ?x) ∧ decode ?x)
          ∧ (∃?y ?wt/2 (?warr/1 = ?y -> ?wt/2).
            ?wu = ?warr/1 ∧ ?wt/2 = ?y ∧ decode ?y)
      in decode_scheme ?final_scheme
    )
  -> let ?final_scheme : ?final_term =
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
    in decode_scheme ?final_scheme
  -> let ?final_scheme : ?final_term =
      ∃?wu.
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
    in decode_scheme ?final_scheme
  -> let ?final_scheme : ?final_term =
      ∃?wu.
        ∃?wt.
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
          ∧ ∃?y ?wt/2 (?warr/1 = ?y -> ?wt/2).
            ?wu = ?warr/1 ∧ ?wt/2 = ?y ∧ decode ?y
    in decode_scheme ?final_scheme
  -> let ?final_scheme : ?final_term =
      ∃?wu.
        ∃?wt.
          ∃?x.
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
          ∧ ∃?y ?wt/2 (?warr/1 = ?y -> ?wt/2).
            ?wu = ?warr/1 ∧ ?wt/2 = ?y ∧ decode ?y
    in decode_scheme ?final_scheme
  -> let ?final_scheme : ?final_term =
      ∃?wu.
        ∃?wt.
          ∃?x.
            ∃?wt/1.
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
          ∧ ∃?y ?wt/2 (?warr/1 = ?y -> ?wt/2).
            ?wu = ?warr/1 ∧ ?wt/2 = ?y ∧ decode ?y
    in decode_scheme ?final_scheme
  -> let ?final_scheme : ?final_term =
      ∃?wu.
        ∃?wt.
          ∃?x.
            ∃?wt/1.
              ∃?warr.
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
                ∧ ∃(?int = int). ?int = ?wt/1 ∧ ?int = ?x
                ∧ decode ?x
          ∧ ∃?y ?wt/2 (?warr/1 = ?y -> ?wt/2).
            ?wu = ?warr/1 ∧ ?wt/2 = ?y ∧ decode ?y
    in decode_scheme ?final_scheme
  -> let ?final_scheme : ?final_term =
      ∃?wu.
        ∃?wt.
          ∃?x.
            ∃?wt/1.
              ∃?warr.
                ⊤
                ∧ hole
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
                       int(0) = int
                       warr |--> wt
                       wt/1 |--> final_term
                       x |--> wu
                       wt(0) = wu -> final_term
                       wu(0)
                       final_term(0)
                }
                (?int = ?wt/1 ∧ ?int = ?x)
                ∧ decode ?x
          ∧ ∃?y ?wt/2 (?warr/1 = ?y -> ?wt/2).
            ?wu = ?warr/1 ∧ ?wt/2 = ?y ∧ decode ?y
    in decode_scheme ?final_scheme
  -> let ?final_scheme : ?final_term =
      ∃?wu.
        ∃?wt.
          ∃?x.
            ∃?wt/1.
              ∃?warr.
                ⊤
                ∧ ∃?int.
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
                         int(0) = int
                         warr |--> wt
                         wt/1 |--> int
                         x |--> wu
                         wt(0) = wu -> int
                         wu(0)
                         final_term |--> int
                  }
                  (?int = ?wt/1)
                  ∧ ?int = ?x
                ∧ decode ?x
          ∧ ∃?y ?wt/2 (?warr/1 = ?y -> ?wt/2).
            ?wu = ?warr/1 ∧ ?wt/2 = ?y ∧ decode ?y
    in decode_scheme ?final_scheme
  -> let ?final_scheme : ?final_term =
      ∃?wu.
        ∃?wt.
          ∃?x.
            ∃?wt/1.
              ∃?warr.
                ⊤
                ∧ ∃?int.
                  ⊤
                  ∧ hole
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
                         int(0) = int
                         warr |--> wt
                         wt/1 |--> int
                         x |--> int
                         wt(0) = int -> int
                         wu |--> int
                         final_term |--> int
                  }
                  (?int = ?x)
                ∧ decode ?x
          ∧ ∃?y ?wt/2 (?warr/1 = ?y -> ?wt/2).
            ?wu = ?warr/1 ∧ ?wt/2 = ?y ∧ decode ?y
    in decode_scheme ?final_scheme
  -> let ?final_scheme : ?final_term =
      ∃?wu.
        ∃?wt.
          ⊤
          ∧ hole
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
                 y(0)
                 int(0) = int
                 warr |--> wt
                 wt/1 |--> int
                 x |--> int
                 wt(0) = int -> int
                 wu |--> int
                 final_term |--> int
          }
          (∃?wt/2 (?warr/1 = ?y -> ?wt/2). ?wu = ?warr/1 ∧ ?wt/2 = ?y ∧ decode ?y)
    in decode_scheme ?final_scheme
  -> let ?final_scheme : ?final_term =
      ∃?wu.
        ∃?wt.
          ⊤
          ∧ ∃?y.
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
                   wt/2(0)
                   y(0)
                   int(0) = int
                   warr |--> wt
                   wt/1 |--> int
                   x |--> int
                   wt(0) = int -> int
                   wu |--> int
                   final_term |--> int
            }
            (∃(?warr/1 = ?y -> ?wt/2). ?wu = ?warr/1 ∧ ?wt/2 = ?y ∧ decode ?y)
    in decode_scheme ?final_scheme
  -> let ?final_scheme : ?final_term =
      ∃?wu.
        ∃?wt.
          ⊤
          ∧ ∃?y.
            ∃?wt/2.
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
                     warr/1(0) = y -> wt/2
                     wt/2(0)
                     y(0)
                     int(0) = int
                     warr |--> wt
                     wt/1 |--> int
                     x |--> int
                     wt(0) = int -> int
                     wu |--> int
                     final_term |--> int
              }
              (?wu = ?warr/1 ∧ ?wt/2 = ?y ∧ decode ?y)
    in decode_scheme ?final_scheme
  
  File "error_clash_types.test", line 1, characters 23-34:
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
  Input term: lambda x. x x
  Generated constraint:
    let ?final_scheme : ?final_term =
      ∃?x ?wt (?warr = ?x -> ?wt).
        ?final_term = ?warr
        ∧ (∃?wu (?wt/1 = ?wu -> ?wt). ?wt/1 = ?x ∧ ?wu = ?x)
        ∧ decode ?x
    in decode_scheme ?final_scheme
  Constraint solving log:
  -- hole {}
    (
      let ?final_scheme : ?final_term =
        ∃?x ?wt (?warr = ?x -> ?wt).
          ?final_term = ?warr
          ∧ (∃?wu (?wt/1 = ?wu -> ?wt). ?wt/1 = ?x ∧ ?wu = ?x)
          ∧ decode ?x
      in decode_scheme ?final_scheme
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
      let ?final_scheme : ?final_term =
        ∃?x ?wt (?warr = ?x -> ?wt).
          ?final_term = ?warr
          ∧ (∃?wu (?wt/1 = ?wu -> ?wt). ?wt/1 = ?x ∧ ?wu = ?x)
          ∧ decode ?x
      in decode_scheme ?final_scheme
    )
  -> let ?final_scheme : ?final_term =
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
    in decode_scheme ?final_scheme
  -> let ?final_scheme : ?final_term =
      ∃?x.
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
    in decode_scheme ?final_scheme
  -> let ?final_scheme : ?final_term =
      ∃?x.
        ∃?wt.
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
    in decode_scheme ?final_scheme
  -> let ?final_scheme : ?final_term =
      ∃?x.
        ∃?wt.
          ∃?warr.
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
            ∧ ∃?wu (?wt/1 = ?wu -> ?wt). ?wt/1 = ?x ∧ ?wu = ?x
            ∧ decode ?x
    in decode_scheme ?final_scheme
  -> let ?final_scheme : ?final_term =
      ∃?x.
        ∃?wt.
          ∃?warr.
            ⊤
            ∧ hole
            {
              Env :
                warr |--> final_term
                final_term(0) = x -> wt
                wt(0)
                wu(0)
                x(0)
              Pool :
                0 |-->
                   wu(0)
                   warr |--> final_term
                   wt(0)
                   x(0)
                   final_term(0) = x -> wt
            }
            (∃(?wt/1 = ?wu -> ?wt). ?wt/1 = ?x ∧ ?wu = ?x)
            ∧ decode ?x
    in decode_scheme ?final_scheme
  -> let ?final_scheme : ?final_term =
      ∃?x.
        ∃?wt.
          ∃?warr.
            ⊤
            ∧ ∃?wu.
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
                     wt/1(0) = wu -> wt
                     wu(0)
                     warr |--> final_term
                     wt(0)
                     x(0)
                     final_term(0) = x -> wt
              }
              (?wt/1 = ?x ∧ ?wu = ?x)
            ∧ decode ?x
    in decode_scheme ?final_scheme
  -> let ?final_scheme : ?final_term =
      ∃?x.
        ∃?wt.
          ∃?warr.
            ⊤
            ∧ ∃?wu.
              ∃?wt/1.
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
                       wt/1(0) = wu -> wt
                       wu(0)
                       warr |--> final_term
                       wt(0)
                       x |--> wt/1
                       final_term(0) = wt/1 -> wt
                }
                (?wt/1 = ?x)
                ∧ ?wu = ?x
            ∧ decode ?x
    in decode_scheme ?final_scheme
  
  File "selfapp.test", line 1, characters 0-13:
  Error: cycle on constraint variable ?wu

## Regressions

  $ minihell --show-source wu_bug.t
  Input term: let x = (lambda y. (lambda z. y) y) (lambda u. u) in x
  Inferred type: ∀α. α -> α
  Elaborated term:
    let (x : ∀β. β -> β) =
      Λβ.
      (lambda (y : β -> β). (lambda (z : β -> β). y) y)
        (lambda (u : β). u)
    in
      x[α]




## Erroneous programs

  $ minihell $FLAGS --log-solver error_undefined_var.test
  File "error_undefined_var.test", line 1, characters 9-23:
  Fatal error: exception Invalid_argument("Constraint variable 'id' is unbound at this point")
  [2]




## Generator tests

This gives example outputs for my implementation. It is completely
fine if your own implementation produces different (sensible) results.

There are not many programs with size 3, 4 and 5.

  $ minigen --exhaustive --types --size 2 --count 100
  Generated term: lambda (x/5 : α/1). x/5
  Inferred type: ∀α/1. α/1 -> α/1
  


  $ minigen --exhaustive --types --size 3 --count 100
  Generated term: lambda (v/14 : α/7). lambda (u/19 : β/7). v/14
  Inferred type: ∀α/7. ∀β/7. α/7 -> β/7 -> α/7
  
  Generated term: lambda (v/14 : δ/7). lambda (u/19 : γ/7). u/19
  Inferred type: ∀γ/7. ∀δ/7. δ/7 -> γ/7 -> γ/7
  


  $ minigen --exhaustive --types --size 4 --count 100
  Generated term:
    lambda
    (v/6a : δ/2a). lambda (w/86 : β/2b). lambda (z/8c : α/2b). v/6a
  Inferred type:
    ∀δ/2a. ∀α/2b. ∀β/2b. δ/2a -> β/2b -> α/2b -> δ/2a
  
  Generated term:
    lambda
    (v/6a : α/2c). lambda (w/86 : γ/2b). lambda (z/8c : δ/2b). w/86
  Inferred type:
    ∀γ/2b. ∀δ/2b. ∀α/2c. α/2c -> γ/2b -> δ/2b -> γ/2b
  
  Generated term:
    lambda
    (v/6a : δ/2c). lambda (w/86 : γ/2c). lambda (z/8c : β/2c). z/8c
  Inferred type:
    ∀β/2c. ∀γ/2c. ∀δ/2c. δ/2c -> γ/2c -> β/2c -> β/2c
  
  Generated term: lambda (v/6a : α/32). let (z/a0 : α/32) = v/6a in v/6a
  Inferred type: ∀α/32. α/32 -> α/32
  
  Generated term: lambda (v/6a : β/32). let (z/a0 : β/32) = v/6a in z/a0
  Inferred type: ∀β/32. β/32 -> β/32
  
  Generated term: lambda (v/6a : γ/39). (v/6a, v/6a)
  Inferred type: ∀γ/39. γ/39 -> {γ/39 * γ/39}
  
  Generated term:
    lambda
    (v/6a : {δ/40 * α/41}).
      let ((u/d8 : δ/40), (v/d8 : α/41)) = v/6a in v/6a
  Inferred type: ∀δ/40. ∀α/41. {δ/40 * α/41} -> {δ/40 * α/41}
  
  Generated term:
    lambda
    (v/6a : {β/41 * γ/41}).
      let ((u/d8 : β/41), (v/d8 : γ/41)) = v/6a in u/d8
  Inferred type: ∀γ/41. ∀β/41. {β/41 * γ/41} -> β/41
  
  Generated term:
    lambda
    (v/6a : {α/42 * δ/41}).
      let ((u/d8 : α/42), (v/d8 : δ/41)) = v/6a in v/d8
  Inferred type: ∀α/42. ∀δ/41. {α/42 * δ/41} -> δ/41
  
  Generated term:
    let (u/ef : ∀γ/53. γ/53 -> γ/53) =
      Λγ/53. lambda (z/104 : γ/53). z/104
    in
      u/ef[β/53]
  Inferred type: ∀β/53. β/53 -> β/53
  


An example of random sampling output at higher size.

  $ minigen --seed 42 --types --size 6 --count 10
  Generated term:
    lambda
    (z/1 : δ/bd).
      lambda
      (x/2a : {β/bd * γ/bd}).
        lambda
        (u/7a : α/bd). let ((x/290 : β/bd), (y/290 : γ/bd)) = x/2a in u/7a
  Inferred type:
    ∀β/bd. ∀γ/bd. ∀α/bd. ∀δ/bd.
      δ/bd -> {β/bd * γ/bd} -> α/bd -> α/bd
  
  Generated term:
    lambda
    (z/1 : β/f9).
      (lambda (x/345 : α/f9). lambda (y/345 : δ/f8). y/345, z/1)
  Inferred type:
    ∀β/f9. ∀δ/f8. ∀α/f9. β/f9 -> {α/f9 -> δ/f8 -> δ/f8 * β/f9}
  
  Generated term:
    lambda
    (z/1 : {γ/fa * β/fb}).
      let ((u/34f : γ/fa), (v/34f : β/fb)) = z/1 in
        lambda (w/34f : α/fb). lambda (x/351 : δ/fa). u/34f
  Inferred type:
    ∀γ/fa. ∀δ/fa. ∀α/fb. ∀β/fb.
      {γ/fa * β/fb} -> α/fb -> δ/fa -> γ/fa
  
  Generated term:
    (
      lambda (u/3e2 : β/128). lambda (v/3e2 : γ/128). u/3e2,
      lambda (w/3e2 : δ/128). w/3e2
    )
  Inferred type:
    ∀δ/128. ∀β/128. ∀γ/128.
      {β/128 -> γ/128 -> β/128 * δ/128 -> δ/128}
  
  Generated term:
    lambda
    (z/1 : α/14a). let (x/1a : {α/14a * α/14a}) = (z/1, z/1) in x/1a
  Inferred type: ∀α/14a. α/14a -> {α/14a * α/14a}
  
  Generated term:
    lambda
    (z/1 : {{β/14f * γ/14f} * δ/14f}).
      let ((u/456 : {β/14f * γ/14f}), (v/456 : δ/14f)) = z/1 in
        let ((w/456 : β/14f), (x/458 : γ/14f)) = u/456 in w/456
  Inferred type:
    ∀γ/14f. ∀δ/14f. ∀β/14f. {{β/14f * γ/14f} * δ/14f} -> β/14f
  
  Generated term: lambda (z/1 : β/169). ((z/1, z/1), z/1)
  Inferred type: ∀β/169. β/169 -> {{β/169 * β/169} * β/169}
  
  Generated term:
    lambda
    (z/1 : {γ/16e * δ/16e}).
      let ((x/4b3 : γ/16e), (y/4b3 : δ/16e)) = z/1 in
        let (z/4b2 : γ/16e) = x/4b3 in z/1
  Inferred type:
    ∀γ/16e. ∀δ/16e. {γ/16e * δ/16e} -> {γ/16e * δ/16e}
  
  Generated term:
    let
    (w/4 : ∀δ/171. ∀α/172. ∀β/172.
      δ/171 -> β/172 -> α/172 -> δ/171)
    =
      Λδ/171. Λα/172. Λβ/172.
      lambda
      (y/31 : δ/171). lambda (z/30 : β/172). lambda (x/81 : α/172). y/31
    in
      w/4[α/171, β/171, γ/171]
  Inferred type:
    ∀γ/171. ∀β/171. ∀α/171. α/171 -> γ/171 -> β/171 -> α/171
  
  Generated term:
    lambda
    (z/1 : δ/2bf). let (x/1a : {δ/2bf * δ/2bf}) = (z/1, z/1) in z/1
  Inferred type: ∀δ/2bf. δ/2bf -> δ/2bf
  

  $ dune exec -- minigen --exhaustive --types --size 10 --count 1
  Generated term:
    (lambda
    (
      x/19dab
      :
        (δ/bea8 -> β/bea9 -> α/bea9 -> δ/bea8)
        ->
        δ/bea8 -> β/bea9 -> α/bea9 -> δ/bea8
    ). x/19dab)
      (lambda (w/1a9ce : δ/bea8 -> β/bea9 -> α/bea9 -> δ/bea8). w/1a9ce)
      (lambda
      (v/1aa39 : δ/bea8).
        lambda (w/1aa55 : β/bea9). lambda (z/1aa5b : α/bea9). v/1aa39)
  Inferred type:
    ∀δ/bea8. ∀α/bea9. ∀β/bea9.
      δ/bea8 -> β/bea9 -> α/bea9 -> δ/bea8
  


## Choice paths

Choice paths make it possible to restart the exhaustive enumeration of minigen in an arbitrary position, instead of restarting from the first term of the given size. This is useful for bug reproducibility purposes.

(We could support the same option for random rather than exhaustive enumeration, but this requires more work on MRand which we have not done for now.)

We can use the option `--log-choice-path` to print the "choice path" for each enumerated element:

  $ minigen --size 4 --exhaustive --count 1000 --log-choice-path
  Choice path: (2 .) (2 .) (2 .) (0 0 .) .
  Generated term:
    lambda
    (v/6a : δ/2a). lambda (w/86 : β/2b). lambda (z/8c : α/2b). v/6a
  
  Choice path: (2 .) (2 .) (2 .) (0 1 .) .
  Generated term:
    lambda
    (v/6a : α/2c). lambda (w/86 : γ/2b). lambda (z/8c : δ/2b). w/86
  
  Choice path: (2 .) (2 .) (2 .) (0 2 .) .
  Generated term:
    lambda
    (v/6a : δ/2c). lambda (w/86 : γ/2c). lambda (z/8c : β/2c). z/8c
  
  Choice path: (2 .) (3 .) (0 0 .) (0 0 .) .
  Generated term: lambda (v/6a : α/32). let (z/a0 : α/32) = v/6a in v/6a
  
  Choice path: (2 .) (3 .) (0 0 .) (0 1 .) .
  Generated term: lambda (v/6a : β/32). let (z/a0 : β/32) = v/6a in z/a0
  
  Choice path: (2 .) (4 (0 .) .) (0 0 .) (0 0 .) .
  Generated term: lambda (v/6a : γ/39). (v/6a, v/6a)
  
  Choice path: (2 .) (5 (0 .) .) (0 0 .) (0 0 .) .
  Generated term:
    lambda
    (v/6a : {δ/40 * α/41}).
      let ((u/d8 : δ/40), (v/d8 : α/41)) = v/6a in v/6a
  
  Choice path: (2 .) (5 (0 .) .) (0 0 .) (0 1 .) .
  Generated term:
    lambda
    (v/6a : {β/41 * γ/41}).
      let ((u/d8 : β/41), (v/d8 : γ/41)) = v/6a in u/d8
  
  Choice path: (2 .) (5 (0 .) .) (0 0 .) (0 2 .) .
  Generated term:
    lambda
    (v/6a : {α/42 * δ/41}).
      let ((u/d8 : α/42), (v/d8 : δ/41)) = v/6a in v/d8
  
  Choice path: (3 .) (2 .) (0 0 .) (0 0 .) .
  Generated term:
    let (u/ef : ∀γ/53. γ/53 -> γ/53) =
      Λγ/53. lambda (z/104 : γ/53). z/104
    in
      u/ef[β/53]
  
We can then restart from an arbitrary point in the enumeration using the `--start-choice-path <string>` option:

  $ minigen --size 4 --exhaustive --count 1000 --start-choice-path "(2 .) (5 (0 .) .) (0 0 .) (0 2 .) ."
  Generated term:
    lambda (x/1 : {α * β}). let ((y/1 : α), (z : β)) = x/1 in y/1
  
  Generated term:
    let (y/18 : ∀δ/11. δ/11 -> δ/11) =
      Λδ/11. lambda (x/2d : δ/11). x/2d
    in
      y/18[γ/11]
  
