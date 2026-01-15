# cl-readr

A Common Lisp port of the R `readr` package, designed to provide fast and friendly functions to read rectangular data (like CSV and TSV files) into `cl-tibble` data frames.

## Quickstart

```lisp
;; Load the system
(ql:quickload :cl-readr)

;; Read a CSV file into a tibble
(defparameter *df* (readr:read-csv "data.csv"))

;; View the tibble
(print *df*)
;; #<TIBBLE:TBL 150x5>

;; Access columns
(cl-tibble:tbl-col *df* "Sepal.Length")

;; Read with options
(readr:read-csv "data.csv" :skip 1 :col-names '("A" "B" "C"))

;; Write back to CSV
(readr:write-csv *df* "output.csv")
```

## Features

-   **Tibble Integration**: All read functions return `cl-tibble` objects, integrating seamlessly with the `cl-tidyverse` ecosystem.
-   **Rectangular Data Parsing**: Supports CSV, TSV, and custom delimiters.

-   **Type Inference**: Automatically guesses column types (Integer, Double, Logical, Character) based on content.
-   **Robust Options**: Supports skipping lines, comments (`#`), max rows (`n-max`), and handling missing values (`NA`).
-   **Writing**: Functions to write tibbles back to CSV or TSV.

## Installation

`cl-readr` is part of the `cl-tidyverse` suite. Ensure you have the following dependencies available:

-   `cl-tibble`
-   `cl-vctrs-lite`

-   `cl-ppcre`
-   `local-time`
-   `alexandria`

Clone the repository to your local-projects directory:

```bash
cd ~/common-lisp/
git clone https://github.com/your-username/cl-readr.git
```

Then load via Quicklisp:

```lisp
(ql:quickload :cl-readr)
```

## API Reference

The package nickname is `readr`.

### Reading Data

#### `read-csv (file &key quote escape col-names skip n-max comment skip-empty-rows)`
Reads a comma-separated values (CSV) file.

-   `file`: Path to the file.
-   `col-names`: `T` (default) to read header from first row. `NIL` to generate names. A list of strings to supply names manually.
-   `skip`: Number of lines to skip before reading.
-   `n-max`: Maximum number of data rows to read.
-   `comment`: A string (e.g., "#") that indicates the start of a comment line.
-   `skip-empty-rows`: If true (default), skip empty lines.

#### `read-tsv (file &key ...)`
Reads a tab-separated values (TSV) file. Same arguments as `read-csv`.

#### `read-delim (file delim &key ...)`
Reads a file with a custom delimiter.
-   `delim`: The character used to separate fields (e.g. `|`).


### Writing Data

#### `write-csv (x file &key delim na append col-names)`
Writes a tibble `x` to a CSV file.
-   `delim`: Delimiter to use (default `,`).
-   `na`: String to use for missing values (default "NA").
-   `append`: If true, append to the file instead of overwriting.
-   `col-names`: If true (default), write headers.

#### `write-tsv (x file &key ...)`
Writes a tibble `x` to a TSV file.


### Helper Functions

#### `read-file (file)`
Reads an entire file into a string.

#### `read-lines (file)`
Reads a file into a list of strings (lines).

## License

MIT License.
