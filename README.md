# 🐫 Logic OCamlculus

[![OCaml](https://img.shields.io/badge/ocaml-5.3.0-orange.svg)](https://ocaml.org/releases/ocaml-5.3.html)

Implementation of selected concepts from propositional logic, first-order logic, logic programming, probabilistic logic programming, and fuzzy logic programming. 


### Definitions and AUX Modules

The file `definitions.ml` contains the foundational type definitions and auxiliary functions utilized by subsequent implementations. This module serves as the core dependency for multiple files. The files under `aux/` folder are a collection of auxiliary functions for their respective modules (for example, `aux/aux_propositional.ml` contains auxiliary functions for propositional logic).


## Propositional Logic


### Normal Forms

The module `propositional/nf.ml` implements recursive transformation algorithms for propositional logic expressions. It provides methods to convert expressions into three canonical forms: Negative Normal Form (NNF), Conjunctive Normal Form (CNF) and Disjunctive Normal Form (DNF).


### Proof Procedures

`propositional/` folder also implements the following proof methods and some of the auxiliary functions for each method:
1. Resolution 
<img src="images_for_README/resolution_propositional.png" width="500" height="300" alt="Resolution Procedure Pseudocode is supposed to be displayed here">

2. Sequent Calculus
<img src="images_for_README/sequent_calculus_propositional.png" width="500" height="300" alt="The Gentzen System G' is supposed to be displayed here">

3. Tableaux (Semantic and Analytic)
<img src="images_for_README/semantic_tableaux_propositional.png" width="500" height="300" alt="Semantic Tableaux for Propositional Logic is supposed to be displayed here">

<img src="images_for_README/ab_tableaux_propositional.png" width="500" height="300" alt="Alpha-Beta Tableaux for Propositional Logic is supposed to be displayed here">

<img src="images_for_README/a_expansion_tableaux_propositional.png" width="500" height="300" alt="Alpha Expansion Tableaux for Propositional Logic is supposed to be displayed here">

<img src="images_for_README/b_expansion_tableaux_propositional.png" width="500" height="300" alt="Beta Expansion Tableaux for Propositional Logic is supposed to be displayed here">

<img src="images_for_README/analytic_tableaux_propositional_1.png" width="500" height="300" alt="Analytic Tableaux for Propositional Logic 1 is supposed to be displayed here">

<img src="images_for_README/analytic_tableaux_propositional_2.png" width="500" height="300" alt="Analytic Tableaux for Propositional Logic 2 is supposed to be displayed here">


## First-Order Logic

The `first_order/` logic implementation extends the propositional system with quantifiers, variables, functions, and predicates. Key components include:
- **Terms**: Variables, constants, and function applications
- **Formulas**: Atomic formulas, logical connectives, and quantifiers
- **Substitutions**: Variable replacements with terms
- **Structures**: Domain with function and predicate interpretations


### Clausification

The module `first_order/clausification_first_order.ml` implements the complete clausification process:
- **PNF Transformation**: Converts formulas to Prenex Normal Form
- **Skolemization**: Eliminates existential quantifiers using Skolem functions
- **CNF Transformation**: Converts the matrix to Conjunctive Normal Form
- **Clause Extraction**: Produces sets of clauses for resolution

<img src="images_for_README/clausification_first_order.png" width="500" height="300" alt="Clausification Process for First-Order Logic">


### Unification

The module `first_order/unification_first_order.ml` implements:
- **Most General Unifier (MGU)**: Finds the most general substitution that unifies terms
- **Occurs Check**: Prevents infinite unifications
- **Pattern Matching**: One-way unification

<img src="images_for_README/unification_first_order_1.png" width="500" height="300" alt="Unification Algorithm Part 1"> 

<img src="images_for_README/unification_first_order_2.png" width="500" height="300" alt="Unification Algorithm Part 2"> 

<img src="images_for_README/unification_first_order_3.png" width="500" height="300" alt="Unification Algorithm Part 3"> 

<img src="images_for_README/unification_first_order_4.png" width="500" height="300" alt="Unification Algorithm Part 4"> 

<img src="images_for_README/unification_first_order_5.png" width="500" height="300" alt="Unification Algorithm Part 5">


### Proof Procedures 

`first_order/` folder also implements the following proof methods and some of the auxiliary functions for each method:

1. Resolution

<img src="images_for_README/resolution_first_order.png" width="500" height="300" alt="Resolution Procedure (FO) Pseudocode is supposed to be displayed here">


2. Tableaux

<img src="images_for_README/semantic_tableaux_first_order.png" width="600" height="300" alt="Semantic Tableaux (FO) Procedure is supposed to be displayed here">

<img src="images_for_README/fv_tableaux_first_order.png" width="500" height="300" alt="Free Variable Tableaux Procedure is supposed to be displayed here">


3. Sequent Calculus

<img src="images_for_README/sequent_calculus_first_order.png" width="500" height="300" alt="First Order Sequent Calculus Updated Genzen System is supposed to be displayed here">


## Logic Programming

The module `SLD_resolution.ml` implements SLD (Selective Linear Definite clause) resolution:

<img src="images_for_README/sld_resolution.png" width="500" height="300" alt="SLD Resolution">


## Executing the Tests

Simply run this command to compile the code:

```
ocamlc -I aux -I propositional -I first_order -o tests definitions.ml aux/aux_propositional.ml aux/aux_first_order.ml examples.ml propositional/nf.ml propositional/resolution_propositional.ml propositional/sequent_calculus_propositional.ml propositional/tableaux_propositional.ml first_order/clausification_first_order.ml first_order/unification_first_order.ml first_order/resolution_first_order.ml first_order/tableaux_first_order.ml first_order/sequent_calculus_first_order.ml SLD_resolution.ml tests.ml
```

and then run the tests:

```
./tests
```

---

For the sections below, only minimal work has been done, as opposed to propositional and first-order logics above.

## Linear Temporal Logic (LTL)

<img src="images_for_README/LTL.jpg" width="500" height="300" alt="Temporal Operators">

```
ocamlc -o ltl LTL.ml && ./ltl
```

## Fuzzy Logic

```
ocamlc -o fuzzy fuzzy_logic.ml && ./fuzzy
```

## Term Rewriting

```
ocamlc -o term_rewriting term_rewriting.ml && ./term_rewriting
```

## Bonus: Beta Reduction from Lambda Calculus

<img src="images_for_README/BetaReduction.jpg" width="500" height="300" alt="Beta Reduction">

The implementation of Beta Reduction (Lambda Calculus) took only a single file: `Lambda Calculus - Beta Reduction.ml`. The image above is from [piedeleu.com](https://piedeleu.com/posts/diagrammatic-lambda-calculus/).
