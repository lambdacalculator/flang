# FLang

**FLang** is a pedagogical Haskell library for exploring **Formal Languages**, **Regular Expressions**, and **Automata Theory**. It is designed to bridge the gap between theoretical concepts (derivatives, state machines) and executable code, making it an ideal companion for students and educators.

## 🚀 Quick Start

### 1. Installation

Clone the repository and build using Cabal:

```bash
git clone https://github.com/lambdacalculator/flang.git
cd flang
cabal build
cabal repl
```

### 2. "Hello World" (Regular Expressions)

Inside `cabal repl`:

```haskell
import FLang

-- 1. Using Constructors (ACUI Normalized)
-- (a + b)* . a . b
let r1 = star (Let 'a' <+> Let 'b') <.> Let 'a' <.> Let 'b'

-- 2. Using explicit constructors (Pedagogical)
let r2 = Star (Cat (Union (Let 'a' ) (Let 'b')) (Cat (Let 'a') (Let 'b')))

-- 3. Using String Parsing (Postfix)
let r3 = toRE "ab+*ab.."

-- Displaying
print (Compact r1) -- "(a+b)*ab"

-- Test matching
match3 r1 "ab"      -- True
match3 r1 "aaab"    -- True
match3 r1 "ba"      -- False
```

### 3. FSM Construction & Minimization

Convert regexes to Finite State Machines to visualize them:

```haskell
-- Convert regex to FSM using Brzozowski derivatives
let m = brzozowski r1

-- Convert states to integers for simpler viewing
let im = intify m

-- Minimize the machine
let minM = minimize im

-- Output Graphviz DOT format
putStrLn (toDot minM)
```
*(Copy the output to [WebGraphviz](http://www.webgraphviz.com/) to see your machine!)*

---

## 🌟 Advanced Features

### Parameterized Alphabets

FLang isn't limited to `Char`! You can define your own alphabets using the `HasSigma` typeclass.

```haskell
-- Define a custom alphabet type for three symbols
newtype ABC = ABC Char deriving (Eq, Ord, Show)

instance HasSigma ABC where
  sigma = map ABC "abc"
  
-- Now you can use algorithms over this alphabet!
```

### Algorithms Implemented

*   **Derivatives**:
    *   `(<\>)`: Smart ACUI-normalized derivative (guarantees finite automata).
*   **Language Operations**:
    *   `memb` (membership), `merge` (union), `cat` (concatenation), `kstar` (Kleene star).
*   **Conversions**:
    *   **RegExp → FSM**: Brzozowski, Thompson, Glushkov constructions.
    *   **EFSM → FSM**: Epsilon elimination (`elim_eps`).
    *   **NFSM → FSM**: Subset construction (`nfsm_to_fsm`).
    *   **FSM → RegExp**: State elimination via Gaussian Elimination.
*   **Minimization**: Moore's Partition Refinement algorithm.
*   **Equivalence**: Tested via QuickCheck properties.

---

## 📂 Module Guide

| Module             | Description                                                                                      |
| ------------------ | ------------------------------------------------------------------------------------------------ |
| `FLang`            | **Start Here**. Top-level export of common types and functions.                                  |
| `FLang.RegExp`     | The core `RegExp` data type and smart constructors (`<+>`, `<.>`).                               |
| `FLang.FSM`        | Definitions for Deterministic (`FSM`), Nondeterministic (`NFSM`), and Epsilon (`EFSM`) machines. |
| `FLang.Algorithms` | The heavy lifting: conversions, minimization, and solving systems of equations.                  |
| `FLang.Language`   | Defines `HasSigma` and utilities for formal languages (lists of strings).                        |

## 🧪 Running Tests

Ensure correctness by running the comprehensive test suite (including QuickCheck properties):

```bash
cabal test
```

## 👩‍💻 Using FLang in Assignments

You don't need to write your code inside this repository! You can keep your assignments separate while still using the FLang library.

### Method 1: The `cabal.project` File (Recommended)

1.  Create a folder for your assignment (e.g., `my-assignment`).
2.  Inside `my-assignment`, create a file named `cabal.project` with the following content:
    ```
    packages: .
              ../path/to/flang
    ```
    *(Replace `../path/to/flang` with the actual relative or absolute path to where you cloned FLang)*
3.  In your assignment's code, simply `import FLang`.
4.  Run `cabal repl` inside `my-assignment`, and FLang will be loaded automatically!

### Method 2: Cabal Install (Global)

You can verify the library is installable globally (though we recommend Method 1 for modifiability):

```bash
cd flang
cabal install --lib
```
Then you can use `cabal repl --build-depends=flang` anywhere.
