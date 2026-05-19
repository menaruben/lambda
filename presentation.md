---
title: A Lambda Calculus Interpreter for FUP
author: Rubén Mena, Raúl Muñoz Pena, Ramón Gallego, Etienne Gündüz
---

What is Lambda Calculus?
---

# (Informal) Definition
> Lambda Calculus is a system for evaluating `lambda terms/expressions` based on
> function abstraction and application and some rules for `reductions`.

# Syntactic Rules
Consists of 3 types of `terms/expressions`: Variables, Abstraction, Application

## Variable
```lambda
x
myvar
whatever42
```
- Any identifier containing only alphanumeric characters

## Abstraction
```lambda
λx.x
λthen.λelse.then
```
- Expressions starting with `λ` denote a function definition
- A `.` separates the parameter from the function body
  - Note: Our interpreter requires explicit currying (one parameter per `λ`)

## Application
```
(f x)
((f x) y)
```
- Evaluated inside parantheses
- Applies a function `f` to an argument `x`
  - Note: our interpreter requires explicit binary structuring with parentheses

## Representation in Haskell
```hs
data Expr
  = Var String
  | Fun String Expr
  | App Expr Expr
  deriving (Show, Eq)
```
<!-- end_slide -->

Basic Control Flow of Our Interpreter
---

```mermaid +render
flowchart
    A[source] --> PRE[preprocess]
    PRE --> B[tokenize]
    B --> C[parse]
    C --> D[validate]
    D --> E[eval]

    subgraph LOOP["eval loop"]
        direction TB

        DONE[done]

        DELTA["δ reduction<br/>expand builtins"]
        ALPHA["α reduction<br/>avoid capture"]
        BETA["β reduction<br/>substitute"]
        ETA["η reduction<br/>simplify"]

        DELTA -->|until stable| ALPHA
        ALPHA --> BETA
        BETA -.->|no redex| ETA
        BETA -->|changed?| DELTA

        ETA --> DONE
    end

    E --> DELTA
```
<!-- end_slide -->

Identifier/Variable Conventions
---

# Naming Scopes and Roles
## Normal Identifiers (User Code)
- Alphanumeric identifiers (e.g.: `x`, `hello`, `whatever42`)
- regular runtime variables and parameter bindings

## Macro Identifiers (Preprocessor)
- Prefixed with a `@` symbol when referenced in code (e.g.: `@not`, `@xor`, `@and`)
- "Solves" C Preprocessor Problem: no accidental replacements

## "Fresh" Identifiers (Alpha Reductions)
- Appends a tick postfix (e.g.: `x'`, `y''`)
- Generated dynamically to avoid identifier capture

## Built-in Identifiers (Delta Reductions)
- Prefixed with a `$` symbol (e.g.: `$f`, `$x`, `$then`, `$else`)
- Automatically expanded (Church Numerals, Booleans)

## Why?
- Traceability Benefit

<!-- end_slide -->
Demonstration
---

Let's go through a few examples containing:
- Alpha Reductions
  - `(λx.λy.x y)`

- Beta Reductions
  - `examples/add_2_3.lambda`

- Eta Reductions
  - `λx.(f x)`

- Delta Reductions
  - Church Numerals
    - `7`
    - `((3 add1) ((2 add1) zero))`

- Booleans / Conditionals
  - `true`  -> `λthen.λelse.then`
  - `false` -> `λthen.λelse.else`
  - Branch evaluation: `(((true hi) bye)` evaluates to `hi`

  - bool-ish operations
    - `examples/not.lambda`
    - `examples/and.lambda`
    - `examples/or.lambda`
    - `examples/xor.lambda`

- Omega Combinator
  - `(λx.(x x) λx.(x x))`

- Y Combinator
  - `λf.(λx.(f (x x)) λx.(f (x x)))`

<!-- end_slide -->

Future Features
---

- A Static Type System
- An actual LSP (language server protocol) implementation for the language
- More builtins (and, or, xor, not, ...)
- Common shorthands for passing multiple arguments or declaring multiple arguments
  - From `\x.\y.\z.(x y z)` to `\x y z.(x y z)`
  - From `(((\x.\y.\z.(x y z) a) b) c)` to `(\x.\y.\z.(x y z) a b c)`

<!-- end_slide -->

The End
---

Thank you for listening!

Are there any questions?
<!-- end_slide -->
