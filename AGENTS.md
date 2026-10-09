# cl-readr Development Agents

## M0: Infrastructure
- **Status**: [ ]
- **Goal**: Set up ASD system, packages, and test harness.
- **Tasks**:
    - [ ] Create `cl-readr.asd` with dependencies (`cl-vctrs-lite`, `cl-tibble`).
    - [ ] Create `src/packages.lisp` exporting initial symbols.
    - [ ] Fix `scripts/` (test, repl) and `Makefile`.
    - [ ] Setup FiveAM test suite framework in `tests/main.lisp`.
    - [ ] VERIFY: `make test` runs and passes (empty tests).

## M1: Basic Reading (Strings/Lines)
- **Status**: [ ]
- **Goal**: Implement low-level reading primitives.
- **Tasks**:
    - [ ] TEST: Write tests for `read-lines` and `read-file`.
    - [ ] Implement `read-file` (read entire file to string).
    - [ ] Implement `read-lines` (read file to list of strings).
    - [ ] VERIFY: Tests pass.

## M2: Delimited Parsing Core
- **Status**: [ ]
- **Goal**: Implement `read-delim` specifically for string splitting and basic tokenization (no types yet, just strings).
- **Tasks**:
    - [ ] TEST: Write tests for `read-delim` with various delimiters (comma, tab, custom).
    - [ ] Implement `read-delim` tokenizer.
    - [ ] Handle `quote` and `escape` characters.
    - [ ] Handle `col-names` (headers).
    - [ ] VERIFY: Tests pass.

## M3: Type Inference & Parsing
- **Status**: [ ]
- **Goal**: Automatically guess column types (int, double) and parse them.
- **Tasks**:
    - [ ] TEST: Write tests for `parse-guess`, `parse-integer`, `parse-double`.
    - [ ] Implement `parse-integer`, `parse-double`, `parse-logical`.
    - [ ] Implement `col-guess` logic (sample first 1000 rows).
    - [ ] Integrate parsing into `read-delim`.
    - [ ] VERIFY: Tests pass.

## M4: Tibble Integration
- **Status**: [ ]
- **Goal**: Return valid `cl-tibble` objects.
- **Tasks**:
    - [ ] TEST: Write tests asserting output class is `cl-tibble:tibble`.
    - [ ] Convert list-of-columns to `cl-tibble` construction.
    - [ ] Ensure `cl-vctrs-lite` types are used for columns.
    - [ ] VERIFY: Tests pass.

## M5: Advanced Options (Robustness)
- **Status**: [ ]
- **Goal**: Handle real-world messy data (NA, comments, skipping).
- **Tasks**:
    - [ ] TEST: Write tests for `na`, `skip`, `n-max`, `comment`, `trim-ws`.
    - [ ] Implement `na` replacement (strings to nil/NA).
    - [ ] Implement `skip` and `n-max`.
    - [ ] Implement `comment` line skipping.
    - [ ] VERIFY: Tests pass.

## M6: Public API Wrappers
- **Status**: [ ]
- **Goal**: Implement standard `read-csv`, `read-tsv`, `read-csv2`.
- **Tasks**:
    - [ ] TEST: Write tests for `read-csv`, `read-tsv` using fixture files.
    - [ ] Implement `read-csv` (wrapper around `read-delim`).
    - [ ] Implement `read-tsv` (wrapper around `read-delim`).
    - [ ] VERIFY: Tests pass.

## M7: Writing
- **Status**: [ ]
- **Goal**: Implement `write-csv`.
- **Tasks**:
    - [ ] TEST: Write tests for `write-csv` (roundtrip test).
    - [ ] Implement `write-csv` for tibbles.
    - [ ] VERIFY: Tests pass.

## M8: Lists/DSL (Lisp Sugar)
- **Status**: [ ]
- **Goal**: Add idiomatic Lisp macros/functions.
- **Tasks**:
    - [ ] TEST: Write tests for `with-csv` and keyword aliases.
    - [ ] Implement `with-csv` macro for row-wise processing.
    - [ ] Refine API for Lisp ergonomics (e.g. `read-csv` accepting keywords for types).
    - [ ] VERIFY: Tests pass.

## Change Discipline (git + documentation)

Every change to this repository must be reconstructible later: what changed,
why, and what it means for users. Agents and humans follow these rules.

1. **Branch, never commit straight to `main`.** Use a topic branch such as
   `fix/<topic>` or `feat/<topic>` (cross-package work in the tidystat effort
   uses `fix/tidystat-phase0`, `feat/tidystat-phase1`, ...).
2. **One logical change per commit.** Test-suite repairs, bug fixes, new
   features and documentation go in separate commits, so each can be
   reviewed, bisected or reverted on its own.
3. **Commit messages explain the why.** Format:
   ```
   <area>: <imperative summary, <= 72 chars>

   Problem:  what was wrong / missing, with the observable symptom.
   Cause:    the root cause (file:function), if it is a fix.
   Change:   what this commit does.
   Impact:   behaviour change for users; mark BREAKING if any.
   Tests:    which tests were added/changed, and the suite result.
   ```
4. **Every commit updates `CHANGELOG.md`** under `## [Unreleased]`, in the
   sections *Fixed*, *Added*, *Changed*, *Breaking*, *Tests*. Each entry
   says what users will notice and names the commit topic.
5. **Run the full test suite before committing** (`make test`, or from the
   umbrella repo `../cl-tidystat/scripts/run-tests.sh <this-package>`) and put
   the result in the commit message. Never commit with a red suite unless
   the commit message says so and why.
6. **Cross-package changes** (e.g. an API in `cl-vctrs-lite` used by
   `cl-dplyr`) are recorded in both repos' changelogs and in
   `../cl-tidystat/docs/CHANGES-phase*.md`, which links the commits.
7. **Push only after review**; do not rewrite history that has been pushed.
