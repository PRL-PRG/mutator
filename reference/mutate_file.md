# Generate Mutants for a Single R File

Creates mutants for a single R source file by combining AST-based
mutations from the C++ mutation engine with fallback line-deletion
mutants.

## Usage

``` r
mutate_file(
  src_file,
  out_dir = "mutations",
  max_mutants = NULL,
  max_line_deletions = 5
)
```

## Arguments

- src_file:

  Path to an R source file.

- out_dir:

  Directory where mutant files are written.

- max_mutants:

  Optional cap on the number of returned mutants. If set, a random
  subset of generated mutants is returned.

- max_line_deletions:

  Maximum number of line-deletion mutants generated per file (a random
  subset of deletable lines). These complement the AST-based statement
  deletions by also covering top-level / non-block lines. Use `0` to
  disable line-deletion mutants entirely. Defaults to `5`.

## Value

A list of mutants. Each element contains:

- `path`:

  Path to the mutant file.

- `info`:

  Formatted mutation metadata (file, source range, and details).

- `loc`:

  Machine-readable location: a list with `file_path`, `start_line`, and
  `end_line` (the latter two `NA` when unavailable).

## Examples

``` r
src <- tempfile(fileext = ".R")
writeLines("add <- function(x, y) x + y", src)
mutants <- mutate_file(src, out_dir = tempfile("mutations_"), max_mutants = 1)
#> Generated 1 AST-based mutants for file1aac3358ca11.R
length(mutants)
#> [1] 1
```
