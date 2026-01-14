# FLang Roadmap

Ideas for future enhancements and features to prepare `flang` for Hackage and the classroom.

## 1. Boolean Transducers & Homomorphisms

Implement a `Transducer` machine type that outputs a string (or element of a monoid) for every transition.

* **Concept**: Generalization of homomorphisms.

* **Type**: `state -> char -> (state, output_string)`

* **Properties**: Regular languages preserved under direct/inverse image.

* **Relation to FSM**: FSM is a transducer over the Boolean monoid.

## 2. Language Operations on Infinite Lists (LOL)

Expand the `FLang.Language` module (which treats languages as length-ordered sorted lists) to include standard closure properties.

* **Set Operations**:
    * Intersection (`intersect :: Lang a -> Lang a -> Lang a`)
    * Set Difference (`diff :: Lang a -> Lang a -> Lang a`)
    * Complement (requires careful handling of the alphabet)
* **Structural Operations**:
    * Reverse
* **Quotients**:
    * Left/Right quotient by a character.
    * Left/Right quotient by a string.
    * Left/Right quotient by a *finite* set of strings.
    * *Note*: Quotients by infinite languages are excluded as determining "no further candidates" is undecidable or computationally infeasible for streams.

## 3. Optimization and Data Structures

* **Data.Set**: Replace `[a]` lists with `Data.Set a` in `NFSM` transitions and closure algorithms (`uclosure`) to improve performance from $O(N^2)$ to $O(N \log N)$.

* **Partial Transitions**: Consider `Maybe a` return types for dense representations or map-based transitions for sparse representations.

## 4. Infinite Machines Support

* **Context-Free Parsing**: Demonstrate parsing Greibach Normal Form grammars using infinite nondeterministic machines.



## 6. Robust Input Parsing (Infix Support)

The current `toRE` is a pedagogical postfix parser. To make the library approachable for students familiar with standard regex syntax:

* **Goal**: Implement a standard infix parser.

* **Features**:

  * Standard operator precedence (`*` > `.` > `+`).

  * Parentheses support.

  * Syntactic sugar: Character ranges (`[a-z]`), wildcard (`.`), optional (`?`), plus (`+`).

## 7. High-Quality Visualization (LaTeX/TikZ)

Graphviz (`toDot`) is great for quick debugging, but high-quality lecture notes and student papers require LaTeX.

* **Goal**: Implement `toTikZ :: Machine m => m -> String`.

* **Style**: Generate code compatible with the standard LaTeX `automata` library (TikZ).

## 8. Pedagogical Tracers

Algorithms like Gaussian Elimination (`solve`) and Glushkov construction are often "black boxes" to students.

* **Goal**: Add `Debug` variants of core algorithms that output the intermediate steps.

* **Example**: `solveTrace` that prints the matrix state after eliminating each variable, showing the application of Arden's Lemma in real-time.

## 9. Interactive REPL (CLI)

A standalone executable (`flang-repl`) that allows students to manipulate regexes without needing to learn GHCi or Haskell syntax first.

* **Commands**: `:load`, `:derive`, `:minimize`, `:equiv`, `:draw`.
