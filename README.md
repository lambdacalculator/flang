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

You don't need to write your code inside this repository! There are two ways to use FLang for your assignments.

### Option 1: Modern Project-Based Setup (Recommended)

This is the most robust method. It avoids global environment conflicts and is easier to manage as the semester progresses.

1.  **Clone the repository**: `git clone https://github.com/lambdacalculator/flang.git`
2.  **Create a folder** for your assignments (e.g., `~/cs215/homework`).
3.  **Create a `cabal.project` file** in your homework folder with the following content:
    ```haskell
    packages: .
    optional-packages: ../flang/flang.cabal
    ```
    *(Note: Adjust the path `../flang/` to point to where you cloned the library)*
4.  **Start your homework files** (e.g., `Assign1.hs`) with this 3-line header:
    ```haskell
    {- cabal:
    build-depends: base, flang
    -}
    module Assign1 where
    import FLang
    ```
5.  **Run or interact** using `cabal`:
    *   To start GHCi: `cabal repl Assign1.hs`
    *   To run compiled: `cabal run Assign1.hs`

### Option 2: Global Installation (Legacy)

This method installs FLang into your system's global Haskell environment. It's simpler to start but can occasionally be brittle if you have multiple versions installed.

1.  **Clone the repository**: `git clone https://github.com/lambdacalculator/flang.git`
2.  **Navigate into the folder**: `cd flang`
3.  **Install the library**: `cabal install --lib . --overwrite-policy=always`

Once installed, you can `import FLang` from any Haskell file on your system and load it directly with `ghci MyFile.hs`.

### 🔄 Updating the Library

If your instructor pushes updates to FLang:

1.  Navigate to your cloned `flang` folder.
2.  Run the update script:
    ```bash
    ./update.sh
    ```
    *(This script automatically pulls changes and re-installs the library for you.)*


