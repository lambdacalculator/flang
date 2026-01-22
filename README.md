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
-- Note: We generally use 'Char' for regexes, but algorithms like 'minimize'
-- require a finite alphabet (HasSigma). We can map chars to the 'AB' type.
let r_ab = fmap AB r1 
let m = brzozowski r_ab

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

FLang includes built-in types `AB` (alphabet {a,b}) and `ABC` (alphabet {a,b,c}). 
For custom alphabets, you can define your own using the `HasSigma` typeclass.

```haskell
-- Define a custom alphabet type for three numeric symbols
newtype Nums = Nums Char deriving (Eq, Ord, Show)

instance HasSigma Nums where
  sigma = map Nums "123"
  
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

### Method for Assignments (The "Environment File" Method)

This is the most robust way to work on assignments. It creates a configuration file that lets you use plain `ghci` with FLang pre-loaded.

1.  **Create your folder**: Make a directory for your assignment (e.g., `my-assignment`) alongside your `flang` clone.
2.  **Point to FLang**: Inside `my-assignment`, create a file named `cabal.project` containing:
    ```
    packages: .
              ../flang
    ```
    *(Adjust `../flang` if your directories are organized differently. Windows users: use forward slashes `/` or double backslashes `\\`)*
3.  **Generate Environment**: Open your terminal in `my-assignment` and run:
    ```bash
    cabal install --lib flang --package-env .
    ```
    *(This creates a hidden `.ghc.environment...` file)*
4.  **Work**: Now you can simply run `ghci`!
    ```bash
    $ ghci
    GHCi> import FLang
    GHCi> :load Assignment.hs
    ```

**Troubleshooting**:
*   If you see "can't find file", make sure you are in the correct directory.
*   If `import FLang` fails, delete the `.ghc.environment` file and run step 3 again.
*   **Do not** use `cabal repl flang` (this opens the library itself, not your assignment). Just use `ghci`.

## 🔄 Updating the Library

If your instructor pushes updates to FLang, getting them is easy:

1.  Navigate to your `flang` folder: `cd flang`
2.  Pull the latest changes: `git pull`
3.  (Optional) If new files were added, you might need to refresh your environment in your assignment folder:
    ```bash
    cd ../my-assignment
    rm .ghc.environment*  # Delete the old environment file
    cabal install --lib flang --package-env .
    ```

