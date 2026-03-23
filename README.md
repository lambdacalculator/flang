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

You don't need to write your code inside this repository! FLang can be installed globally so you can use it from any folder on your computer.

### Installation

To install FLang globally on your system:

1.  **Clone the repository**: `git clone https://github.com/lambdacalculator/flang.git`
2.  **Navigate into the folder**: `cd flang`
3.  **Install the library**: `cabal install --lib .`

*(Note: this installs the library into your user-level Cabal environment, not system-wide)*

### Working on Assignments

Once installed, you can create a new folder anywhere for your assignments (e.g., `~/119/my-assignment`). Inside your assignment file (e.g., `Assign1.hs`), simply include the import statement at the top:

```haskell
module Assign1 where

import FLang
```

You can then open your terminal in that folder and load your file directly:

```bash
$ ghci Assign1.hs
```

### 🔄 Updating the Library

If your instructor pushes updates to FLang, getting them is easy. We have provided a script that pulls the latest code and reinstalls the library for you.

Simply navigate to your cloned `flang` folder and run the update script:

```bash
cd flang
./update.sh
```

*(Alternatively, you can manually run `git pull` followed by `cabal install --lib .`)*


