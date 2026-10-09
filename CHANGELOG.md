# Changelog

All notable changes to this project are documented here. Format follows
[Keep a Changelog](https://keepachangelog.com/en/1.1.0/). See AGENTS.md
("Change Discipline") for how entries and commits are written.

## [Unreleased]

### Security
- **CSV cells could execute arbitrary Lisp code.** `true-double-p` (used by
  type guessing on every cell) called `read-from-string` without binding
  `*read-eval*` to NIL, so a cell such as `#.(delete-file ...)` was
  evaluated while reading the file. Cells are now only passed to the
  reader after a strict syntactic check for a plain decimal number
  (`[+-]?(d+[.d*]|.d+)([eEdD][+-]?d+)?`), under `with-standard-io-syntax`
  with `*read-eval*` NIL. Text cells also no longer intern symbols
  (topic: safe number parsing).

### Fixed
- Doubles are parsed at double precision. They were read with the default
  single-float format and then widened, so `"0.1"` became
  `0.10000000149011612d0`. Now `"0.1"` → `0.1d0`.
- Integer guessing no longer accepts junk via `:junk-allowed`; surrounding
  blanks are still accepted (`" 7 "` → 7).
