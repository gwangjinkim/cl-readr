# cl-readr Specification

`cl-readr` is a Common Lisp port of the R `readr` package. It provides a fast and friendly way to read rectangular data (like CSV, TSV, and fixed-width files).

## Core Philosophy
- **Speed**: Efficient parsing of large files.
- **Friendly**: Helpful error messages and smart defaults.
- **Reproducible**: Consistent behavior across environments.
- **Tibble Output**: Returns `cl-tibble` objects, not just lists or arrays.

## Dependencies
- `cl-vctrs-lite`: For underlying vector types.
- `cl-tibble`: For the data frame structure.
- `cl-dplyr` & `cl-tidyr`: (Integration points).

## API Specification

### Reading Functions

#### `read-delim`
The general function for reading delimited files.
```lisp
(read-delim file delim &key quote escape col-names col-types 
            locale na comment trim-ws skip n-max progress skip-empty-rows)
```

#### `read-csv` / `read-csv2`
Wrappers for comma-separated (and semicolon-separated) files.
```lisp
(read-csv file &key ...)
```

#### `read-tsv`
Wrapper for tab-separated files.

#### `read-file` / `read-lines`
Utilities to read a file into a single string or a list of lines.

### Column Specification (`col-types`)
A mechanism to specify column types manually or let the parser guess.

- **`col-guess`** (Default): Guesses type based on the first 1000 rows.
- **`col-character`**: Read as strings.
- **`col-integer`**: Read as integers.
- **`col-double`**: Read as double floats.
- **`col-logical`**: Read as booleans (T/F, TRUE/FALSE, etc).
- **`col-date` / `col-datetime`**: Parse dates (ISO8601 default).
- **`col-skip`**: Don't import this column.

### Parsing Features
- **NA Handling**: Configurable strings treated as missing values (default `("NA" "")`).
- **Quoting**: Handling quoted fields correctly.
- **Comments**: Lines starting with a comment character are skipped.
- **Skipping**: Skip the first N lines.

## Output Format
Returns a `cl-tibble:tibble` object.
- Column types are preserved (using `cl-vctrs-lite` classes).
- Header names are preserved (not mangled).

## DSL (Lisp-like extensions)
A "Sugar API" to make reading more idiomatic in Lisp.

```lisp
(readr:with-csv (row "data.csv")
  (print row)) ;; Iterative processing?

(readr:read-csv "data.csv"
  :types '(:name :string :age :int))
```
