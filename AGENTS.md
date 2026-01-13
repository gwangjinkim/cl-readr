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
