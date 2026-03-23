# Revision history for flang

## 0.1.1.0 -- 2026-03-06
* Export all matchers, FSM constructions, and algorithms in `FLang.hs`.
* Optimize `compile` (`MProg`) to use dual read/epsilon continuations, resolving catastrophic exponential AST expansion during compilation.

## 0.1.0.0 -- 2026-01-14
* First version. Released for pedagogical use.
* Implements Brzozowski derivatives with ACUI normalization.
* Implements FSM to RegExp conversion via Gaussian Elimination (Arden's Lemma).
