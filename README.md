# Generating well-typed random terms using constraint-based type inference

## Introduction

The goal of this programming project is to reuse a constraint-based
type inference engine (for a simple lamda-calculus) to implement
a random generator of well-typed term.

Contact: gabriel.scherer@inria.fr .

### Motivation

Random generation of well-typed term has been studied as a research
problem: if you want to apply random fuzzing testing techniques on the
implementation of a programming language, you need to generate a lot
of programs in this language. Generating programs that parse correctly
is not too hard, but generating well-typed programs can be hard -- and
generating ill-typed program does not test the rest of the
implementation very well.

To generate well-typed terms, a common approach is to write a random
generator that "inverts" the rules of the type-system, from the root
of the typing derivation to the leaves. If the generator wants to
generate a term at a given type, it can list the typing rule that may
produce this output type, decide to apply one of them, which
corresponds to choosing the head term-former of the generated program,
and in turn requires generating subterms whose types are given by the
chosen typing rule. For example, if you want to generate the program
(? : A -> B), choosing the typing rule for lambda-abstraction refines
it into (lambda (x : A). (? : B)) with a smaller missing hole. If a type in the derivation cannot be

An example of this brand of work is the following:

  Michał H. Pałka, Koen Claessen, Alejandro Russo, and John Hughes
  Testing an Optimising Compiler by Generating Random Lambda Terms
  2012
  https://publications.lib.chalmers.se/records/fulltext/157525.pdf

This approach (which basically corresponds to a Prolog-style
proof search) is easy for simple type systems, and rapidly becomes
difficult for advanced type systems -- for example, dealing with
polymorphism is either unsatisfying or very complex. It is frustrating
to spend a lot of effort writing a complex generator, because we
(language implementers) are already spending a lot of effort writing
complex type-checkers, and it feels like a duplication of work on
similar problems.

Is it possible to write a type-checker once, and reuse it for random
generation of well-typed programs?

### The project

In this project, you will implement a *simple* constraint-based type
inference engine, basically a miniature version of Inferno
( https://inria.hal.science/hal-01081233 ,
https://gitlab.inria.fr/fpottier/inferno ) for a small simply-typed
lambda-calculus (no ML-style polymorphism), and then turn it into
a random generator of well-typed programs.

We provide you with a skeleton for the project, with most of the
boring stuff already implemented, so that you can focus on the
interesting parts. We also provide code to help you test your
code.

### Grading

The project will be evaluated by:

- checking that the code you provided is correct, by reviewing the
  code and your testsuite results

- evaluating the quality of the code (clarity, documentation, etc.)

- evaluating the coverage of the "mandatory" tasks suggested
  by the provided skeleton

- evaluating the difficulty and originality of the extensions you
  implemented, if any

We would like you to write a short REPORT.md file at the root of the
project, which details what you did and explains any non-obvious point,
with pointers to relevant source files. There is no length requirement
for this REPORT.md file, just include the information that *you* think
is valuable and useful -- please, no boring intro or ChatGPT prose.

### Code reuse and plagiarism

Reusing third-party code is allowed, as long as (1) the code license
allows this form of reuse, and (2) you carefully indicate which parts
of the code are not yours -- mark it clearly in the code itself, and
mention it explicitly in your REPORT.md file.

In particular, please feel free to introduce dependencies on
third-party (free software) packages as long as they are explicit in
the packaging metadata of your project -- so that I can still build
the project.

Including code that comes from someone else without proper credit to
the authors is plagiarism.

### OCaml development setup

Install [Opam](https://opam.ocaml.org/doc/Install.html), the OCaml
package manager, on your system.

If you have never used Opam before, you need to initialize it (otherwise, skip this step):

```
$ opam init
```

For convenience, we setup a [local](https://opam.ocaml.org/blog/opam-local-switches/) Opam distribution, using the following commands:

```
$ opam switch create . --deps-only --with-doc --with-test
$ eval $(opam env)
```

To configure your favorite text editor, see the [Real World OCaml setup](http://dev.realworldocaml.org/install.html#editor-setup).

### Using a different language

We wrote useful support code in OCaml, so it is going to be much
easier to implement the project in OCaml. If you insist, you are of
course free to implement the project in a different programming
language -- any language, as long as I can install a free software
implementation of it on my Linux machine to test your code.

You would still need to read OCaml code to understand the key
ingredients of the projet -- type inference constraints with
elaboration, and random generation -- and transpose them in your
chosen implementation language.

Note: we do *not* expect you to reimplement the full project skeleton
as-is (see more details on what we provide in the "Code organization"
section below). Feel free to provide the minimum amount of support
code to demonstrate/test the interesting bits of the project --
constraint generation with elaboration to an explicitly-typed
representation, constraint solving, and then random generation of
well-typed terms.

## High-level description

This project contains a simple type inference engine in the spirit of
Inferno, with a type `Untyped.term` of untyped terms, a type
`STLC.term` of explicitly-typed terms, a type `('a, 'e) Constraint.t`
of constraints that produce elaboration witnesses of type `'a`.

#### Type inference a la inferno

The general idea is to implement a constraint generator of type

    Untyped.term -> (STLC.term, type_error) Constraint.t

and a constraint solving function of type

    val eval : ('a, 'e) Constraint.t -> ('a, 'e) result

which extracts a result (success or failure) from a normal form
constraint.

By composing these functions together, you have a type-checker for
the untyped language, that produces a "witness of well-typedness" in
the form of an explicitly-typed term -- presumably an annotation of
the original program.

(To keep the project difficulty manageable, our "simple type inference
engine" does not handle ML-style polymorphism, or in fact any sort of
polymorphism. We are implementing type inference for the simply-typed
lambda-calculus.)

#### Abstracting over an effect

But then there is a twist: we add to the language of untyped terms
*and* to the language of constraint a `Do` constructor that represents
a term (or constraint) produced by an "arbitrary effect", where the
notion of effect is given by an arbitrary functor (a parametrized type
with a `map` function). This is implemented by writing all the code
in OCaml modules `Make(T : Utils.Functor)` parametrized over `T`.

The constraint-generation function is unchanged.

    Untyped.term -> (STLC.term, type_error) Constraint.t

For constraint solving, however, new terms of the form
  `Do (p : (a, e) Constraint.t T.t)`
now have to be evaluated. We propose to extend the `eval` function to
a richer type

    val eval : ('a, 'e) Constraint.t -> ('a, 'e) normal_constraint

where a normal constraint is either a success, a failure, or an effectful
constraint computation `('a, 'e) Constraint.t T.t`.

We propose an evaluation rule of the form

    eval E[Do p] = NDo E[p]

where E is an evalution context for constraints, `p` has type
`('a, 'e) Constraint.t T.t` (it is a computation in `T` that
returns constraints), and `E[p]` is defined by lifting the
context-surrounding function
`E[_] : ('a, 'e) Constraint.t -> ('b, 'f) Constraint.t`
through the `T` functor.

#### Two or three effect instances

An obvious instantion of this `T : Functor` parameter is to use the
functor `Id.Empty` of empty (parametrized) types with no
inhabitant. This corresponds to the case where the `Do` constructor
cannot be used, the terms and constraint are pure. In this case `eval`
will simply evaluate the constraint to a result. This is what the
`minihell` test program uses.

The other case of interest for this is when the parameter
`T : Utils.Functor` is in fact a search monad `M : Utils.MonadPlus`.
Then it is possible to define a function

    val gen : depth:int -> ('a, 'e) constraint -> ('a, 'e) result M.t

on top of `eval`, that returns all the results that can be reached by
expanding `Do` nodes using `M.bind`, recursively, exactly `depth`
times. (Another natural choice would be to generate all the terms that
can be reached by expanding `Do` nodes *at most* `depth` times, but
this typically gives a worse generator.)

Finally, to get an actual program generator, you need to instantiate
this machinery with a certain choice of `M : MonadPlus` structure. We
ask you to implement two natural variants:

- `MSeq`, which simply enumerates the finite lists of possible
  results.

- `MRand`, which returns random solutions -- an infinite stream of
  independent randomly-sampled solutions.


## Implementation tasks

In short: implement the missing pieces to get the code working,
following the high-level description above. Then (optionally) think
about extending the project further -- we give some ideas in the
"Extensions" section later on.

In more details, we recommend the following progression (but you do as
you prefer).

0. Creating a version-controlled repository for your project, for
   example by running `git init` in the root directory of the project.
   Even if you program all alone, version control is a must-have for
   any moderately complex project. Whenever you have achieved
   progress, save/commit the current state. Whenever you are able to
   backtrack, erase your last hour of work and try to something
   different, save/commit the current state *before* throwing it
   away. You are welcome.

1. Look at the simply-typed lambda-calculus used as a toy programming
   language, implemented in Untyped.ml (the before-type-inference form
   without types everywhere) and STLC.ml (the explicitly-checked
   version produced by our type inference engine). These are fairly
   standard.

2. Look at the datatype of constraints used for type inference,
   defined in Constraint.ml.

3. Read the testsuite in tests.t/run.t to have an idea of the
   sort of behaviors expected once the project is complete.

   Reading the testsuite will also give you ideas on how to run your
   own tests during your work. Feel free to edit the run.t file, or
   add more test files.

   You might want to save the run.t file somewhere for future
   reference. Then we recommend "promoting" the current output
   (where nothing works because nothing is implemented yet) by running
   `dune runtest` (at the root of the project) and then
   `dune promote`. As you implement more features you should regularly
   run `dune runtest` again, and `dune promote` to save the current
   testsuite output. This will help you track your progress and
   notice behavior changes (improvements or regressions).

   Note: the testsuite outputs are descriptive, not prescriptive. If
   you implement the project differently, you may get different
   outputs that are also correct. You have to read the test output to
   tell if they make sense.

4. Implement a constraint generator in Infer.ml.

   You should be able to test your constraint generator by running
   commands such as:

       dune exec -- minihell --show-constraint tests/id_poly.test

   (Please of course feel free to write your own test files, and
    consider adding them to the testsuite in tests/run.t.)

5. Implement a constraint solver in Solver.ml. (There is also
   a missing function, Structure.merge, that you will have to
   implement to get unification working.)

   You should be able to test your constraint solver by running
   commands such as

       dune exec -- minihell --show-constraint --log-solver tests/id_poly.test

   At this point you have a complete type-checker for the simply-typed
   lambda-calculus. Congratulations!

6. Implement search monads in MSeq.ml and MRand.ml. They should obey
   the MonadPlus interface defined in Utils.ml. The intention is that:

   - MSeq should simply generate the finite sequence of all terms of
     a given size (the order does not matter), while
   - MRand should return an infinite sequence, each element being
     a random choice of term of the given size (we do not expect the
     distribution to be uniform: typically, for each syntactic node,
     each term constructor can be picked with a fixed probability).

   You can implement just one of them before going to the next step,
   or both. You only need one to test your code at first.

6. Implement a term generator in Generator.ml.

   You can test the generator instantiated with MRand by running, for
   example:

       dune exec -- minigen --depth 5 --count 3

   You can test the generator instantiated with MSeq:
   example:

       dune exec -- minigen --exhaustive --depth 5 --count 20

   (On my implementation this only generates 10 terms, as the
   generator only produces 10 different terms at this depth.)

7. At this point you are basically done with the "mandatory" part of
   this project. Please ensure that your code is clean and readable,
   we will take this into account when grading.

   You should now think of implementing extensions of your choice.
   (See the "Extensions" section below for some potential ideas.)


## Skeleton code organization

- `tests.t`: the testsuite. See tests.t/run.t for details.
   This is important.

- `bin/`: two small executable programs that are used to
   test the main logic in `src/`:

   + `minihell`: a toy type-checker

   + `minigen`: a toy generator of well-typed terms

   You should feel free to modify these programs if you wish, but you
   do not need to.

- `src/`: the bulk of the code, that place where we expect you to
   work. `src/*.{ml,mli}` are the more important modules that you will
   have to modify or use directory. `src/support/*.{ml,mli}` has the
   less interesting support code. (Again, feel free to modify the
   support code, but you do not need to.)

   + `Generator.ml,mli`: the random term generator (you only need to
     look at this late in the project, feel free to skip it at first)

   + `Infer.ml,mli`: the type-inference engine, that generates constraints
      with elaboration

   + `MRand.ml,mli`: the random-sampling monad

   + `MSeq.ml,mli`: the list-all-solutions monad

   + `Solver.ml,mli`: the constraint solver

   + `STLC.ml`: the syntax of types and well-typed terms

   + `Structure.ml`: the definition of type-formers
     in the type system. This is used in `STLC.ml`,
     but also in constraints that manipulate types
     containing inference variables.

   + `Unif.ml,mli`: the unification engine. This is fully
     implemented. You will need to understand its interface
     (`Unif.mli`) to solve equality constraints in `Solver.ml`.

   + `Untyped.ml`: the syntax of untyped terms.

   + `Utils.ml`: useful bits and pieces. Feel free
     to add your stuff there.

   + `support/`: the boring modules that help for debugging and
     testing, but you probably do not need to use or touch them
     directly. (Feel free to modify stuff there if you want.)

     * `ConstraintPrinter.ml,mli`: a pretty-printer for constraints

     * `ConstraintSimplifier.ml,mli`: a "simplifier" for constraints,
       that is used to implement the `--log-solver` option of
       `minihell`.

     * `Decode.ml,mli`: reconstructs user-facing types from
       the current state of the unification engine.

     * `Printer.ml`: support code for all pretty-printers.

     * `SatConstraint.ml`: "satisfiable constraints" are constraints
       that do not produce an output (no elaboration to
       well-typed terms). This simpler type (no GADTs in sight) is
       used under the hood for simplification and pretty-printing.

    * `STLCPrinter.ml,mli`: a pretty-printer for explicitly-typed
      terms.

    * `UntypedLexer.mll`: an `ocamllex` lexer for the untyped
      language.

    * `UntypedParser.mly`: a `menhir` grammar for the untyped
      language.

    * `UntypedPrinter.ml,mli`: a pretty-printer for the untyped
       language.

   (Again: if you decide to take risks and implement the project in
   a different language, you do not need to reimplement any of that,
   but then you are on your own for testing and debugging.)


### Extensions

After you are done with the core/mandatory part of the project, we
expect you to implement extensions of your choosing. Surprise us!

In case it can serve as inspiration, here are a few ideas for
extensions.

- You could add some extra features to the programming language
  supported. For example:

   + Base types such as integers and booleans

   + Recursive function definitions.

   + Lists, with a built-in shallow pattern-matching construction

          match .. with
          | [] -> ...
          | x :: xs -> ...

   + n-ary tuples: this sounds easy but it is in fact quite tricky,
     because the strongly-typed GADT of constraints does not make it
     easy to generate a "dynamic" number of existential
     quantifiers. This is a difficult exercise in strongly-typed
     programming.

     (We in fact wrote a short paper on how we approached this problem
     in the context of Inferno, and the difficulties are the same.
     https://inria.hal.science/hal-03145040
     Feel free to reuse our approach if you want.)

   + Patterns and pattern-matching. You get into the same difficulties
     as n-ary tuples, so working on n-ary tuples first is probably
     a good idea.

   + You could implement ML-style parametric polymorphism, with
     generalization during type inference. This is the hardest of the
     extensions mentioned here by far: we have not tried this, and we
     do not know how the interaction between enumeration/generation
     and generalization will work out. (One might need to change the
     term generation to stop going 'in and out' of sub-constraints, or
     at least to be careful in the implementation of ML-style
     generalization to use a (semi-)persistent data structure, a tree
     of generalization regions instead of a stack.)

- You could use the term generator for property-based testing of
  *something*: implement a procedure that manipulates explicitly-typed
  terms -- for example, a semantics-preserving program transformation,
  or a compilation to the programming language of your choice, etc --
  and test the property using random generator.

  (You could probably do some greybox fuzzing by providing an
  alternative search monad using Crowbar:
  https://github.com/stedolan/crowbar )

- You could improve the random-generation strategy by thinking about
  the shape of terms that you want to generate, tweaking the
  distribution, providing options to generate terms using only
  a subset of the rules, etc.

  An important improvement in practice would be to provide some form
  of shrinking.

- You could look at existing work on testing of language
  implementations by random generation, and see if they can be
  integrated in the present framework. For example,

  https://janmidtgaard.dk/papers/Midtgaard-al%3aICFP17-full.pdf

  performs random generation of programs with a custom effect system,
  that determines if a given sub-expression performs an observable
  side-effect or not -- to only generate terms with a deterministic
  evaluation order. Could one extend the inference engine to also
  check that the program is deterministic in this sense, and thus
  replace the custom-built program generator used in this work?
