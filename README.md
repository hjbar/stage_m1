# Generating well-typed random terms using constraint-based type inference

## Introduction

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
it into (lambda (x : A). (? : B)) with a smaller missing hole. If
a type in the derivation cannot be

An example of this brand of work is the following:

> Michał H. Pałka, Koen Claessen, Alejandro Russo, and John Hughes  
> Testing an Optimising Compiler by Generating Random Lambda Terms  
> 2012  
> https://publications.lib.chalmers.se/records/fulltext/157525.pdf

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

In this project, we implement a *simple* constraint-based type
inference engine, basically a miniature version of Inferno
( https://inria.hal.science/hal-01081233 ,
https://gitlab.inria.fr/fpottier/inferno ) for a small
system-F (with ML-style polymorphism), and then turn it
into a random generator of well-typed programs.

See the corresponding technical report
(for simply-typed lambda-calculus) at :
<https://inria.hal.science/hal-04607309>


## High-level description

This project contains a simple type inference engine in the spirit of
Inferno, with a type `Untyped.term` of untyped terms, a type
`F.term` of explicitly-typed terms, a type `('a, 'e) Constraint.t`
of constraints that produce elaboration witnesses of type `'a`.

#### Type inference a la inferno

The general idea is to implement a constraint generator of type

    Untyped.term -> (F.term * F.scheme, type_error) Constraint.t

and a constraint solving function of type

    val eval : ('a, 'e) Constraint.t -> ('a, 'e) result

which extracts a result (success or failure) from a normal form
constraint.

By composing these functions together, you have a type-checker for
the untyped language, that produces a "witness of well-typedness" in
the form of an explicitly-typed term -- presumably an annotation of
the original program.

#### Abstracting over an effect

But then there is a twist: we add to the language of untyped terms
*and* to the language of constraint a `Do` constructor that represents
a term (or constraint) produced by an "arbitrary effect", where the
notion of effect is given by an arbitrary functor (a parametrized type
with a `map` function). This is implemented by writing all the code
in OCaml modules `Make(T : Utils.Functor)` parametrized over `T`.

The constraint-generation function is unchanged.

    Untyped.term -> (F.term, type_error) Constraint.t

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

    val gen : size:int -> ('a, 'e) constraint -> ('a, 'e) result M.t

on top of `eval`, that returns all the results that can be reached by
expanding `Do` nodes using `M.bind`, recursively, exactly `size`
times. (Another natural choice would be to generate all the terms that
can be reached by expanding `Do` nodes *at most* `size` times, but
this typically gives a worse generator.)

Finally, to get an actual program generator, we need to instantiate
this machinery with a certain choice of `M : MonadPlus` structure. We
wrote naive implementations of two natural variants:

- `MSeq`, which simply enumerates the finite lists of possible
  results.

- `MRand`, which returns random solutions -- an infinite stream of
  independent randomly-sampled solutions.

## Code organization

- `tests.t`: the testsuite. See tests.t/run.t for details.

- `bin/`: two small executable programs that are used to
   test the main logic in `src/`:

   + `minihell`: a toy type-checker

   + `minigen`: a toy generator of well-typed terms

- `src/`: the bulk of the code. The more important/interesting
  modules are the following:

   + `Generator.ml,mli`: the random term generator

   + `Infer.ml,mli`: the type-inference engine, that generates constraints
      with elaboration

   + `MRand.ml,mli`: the random-sampling monad

   + `MSeq.ml,mli`: the list-all-solutions monad

   + `Solver.ml,mli`: the constraint solver

   + `F.ml`: the syntax of types and well-typed terms

   + `Structure.ml`: the definition of type-formers
     in the type system. This is used in `F.ml`,
     but also in constraints that manipulate types
     containing inference variables.

   + `Unif.ml,mli`: the unification engine.

   + `Untyped.ml`: the syntax of untyped terms.

   + `Utils.ml`: useful bits and pieces.

   + the following modules help for debugging and testing.
     
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

    * `FPrinter.ml,mli`: a pretty-printer for explicitly-typed
      terms.

    * `UntypedLexer.mll`: an `ocamllex` lexer for the untyped
      language.

    * `UntypedParser.mly`: a `menhir` grammar for the untyped
      language.

    * `UntypedPrinter.ml,mli`: a pretty-printer for the untyped
       language.
