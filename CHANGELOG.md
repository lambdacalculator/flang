# Revision history for flang

## 0.2.2.0 -- 2026-04-05
* Improve `update.sh` robustness with `--force-reinstalls` and `--overwrite-policy=always` to fix Cabal-7145 errors.
* Add modern project-based setup instructions to README.

## 0.2.1.0 -- 2026-04-03
*   Add `toDotE` for displaying EFSMs (Epsilon-NFSMs) with epsilon transitions properly labeled.
*   Clean up repository structure and internal benchmark scripts.

## 0.2.0.0 -- 2026-03-23
*   Public repository split: `flang-private` (development) and `flang` (student).
*   Reset repository history for student clarity.
*   Initial 0.2.x series release.

## 0.1.1.0 -- 2026-03-06
* Export all matchers, FSM constructions, and algorithms in `FLang.hs`.
* Optimize `compile` (`MProg`) to use dual read/epsilon continuations, resolving catastrophic exponential AST expansion during compilation.

## 0.1.0.0 -- 2026-01-14
* First version. Released for pedagogical use.
* Implements Brzozowski derivatives with ACUI normalization.
* Implements FSM to RegExp conversion via Gaussian Elimination (Arden's Lemma).
