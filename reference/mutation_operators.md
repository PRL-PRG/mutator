# List the Mutation Operators

Returns the mutation operators that mutator knows about. Use their ids,
or the names of their families, in the `operators` argument of
[`mutate_file()`](https://prl-prg.github.io/mutator/reference/mutate_file.md)
and
[`mutate_package()`](https://prl-prg.github.io/mutator/reference/mutate_package.md).

## Usage

``` r
mutation_operators()
```

## Value

A data frame with one row per operator and columns `id`, `family`,
`default` (whether it is part of the `"default"` group), `description`
and `example`.

## Selecting operators

`operators` is a character vector read from left to right. Each element
is an operator id, a family name (e.g. `"relational"`), `"default"` or
`"all"`. An element prefixed with `-` removes operators instead of
adding them. If the first element is a removal, the selection starts
from `"default"`. When `operators` is `NULL`, the option
`mutator.operators` is used, and `"default"` if that is unset.

## Examples

``` r
mutation_operators()
#>                     id       family default
#> 1           arith_swap   arithmetic    TRUE
#> 2          arith_extra   arithmetic    TRUE
#> 3             rel_swap   relational    TRUE
#> 4         rel_boundary   relational    TRUE
#> 5           logic_swap      logical    TRUE
#> 6           not_remove      logical    TRUE
#> 7       and_or_operand      logical    TRUE
#> 8          cond_negate control_flow    TRUE
#> 9           cond_force control_flow   FALSE
#> 10           loop_ctrl control_flow    TRUE
#> 11         return_null control_flow    TRUE
#> 12          na_replace    constants    TRUE
#> 13        na_type_swap    constants   FALSE
#> 14          const_null    constants    TRUE
#> 15    const_off_by_one    constants   FALSE
#> 16            value_42    constants   FALSE
#> 17        string_empty    constants   FALSE
#> 18           bool_flip     r_idioms    TRUE
#> 19        super_assign     r_idioms    TRUE
#> 20 scalar_vector_logic     r_idioms   FALSE
#> 21   scalar_vector_fun     r_idioms   FALSE
#> 22           index_ops     r_idioms   FALSE
#> 23           seq_idiom     r_idioms   FALSE
#> 24          drop_idiom     r_idioms   FALSE
#> 25      named_arg_drop     r_idioms    TRUE
#> 26            fun_swap     r_idioms    TRUE
#> 27      fun_swap_extra     r_idioms   FALSE
#> 28         call_unwrap     r_idioms   FALSE
#> 29      error_handling control_flow    TRUE
#> 30         stmt_delete     deletion    TRUE
#> 31         line_delete     deletion    TRUE
#>                                                              description
#> 1                                              Swap arithmetic operators
#> 2                             Swap ^, %% and %/%, and remove unary minus
#> 3                                              Swap relational operators
#> 4                                             Move a comparison boundary
#> 5                                                 Swap logical operators
#> 6                                                      Remove a negation
#> 7                                      Keep only one operand of && or ||
#> 8                                           Negate an if/while condition
#> 9                                 Force an if condition to TRUE or FALSE
#> 10                                                   Swap break and next
#> 11                                      Return NULL instead of the value
#> 12                           Replace a constant with NA of the same type
#> 13                              Replace an NA with an NA of another type
#> 14                                          Replace a constant with NULL
#> 15                                         Add or subtract 1 to a number
#> 16      Replace numbers, assigned values and call results with 42 (or 0)
#> 17                                    Replace a non-empty string with ""
#> 18                          Flip TRUE/FALSE, including argument defaults
#> 19                              Turn a super-assignment into a local one
#> 20                          Swap scalar and vectorised logical operators
#> 21     Swap max/min and their parallel pmax/pmin (two or more arguments)
#> 22                               Swap [[ and [ (single index only for [)
#> 23                                    Replace seq_len/seq_along with 1:n
#> 24                                  Remove drop = FALSE from an indexing
#> 25   Drop an argument such as na.rm, sep or fixed so its default applies
#> 26    Swap paired functions (any/all, min/max, head/tail, sub/gsub, ...)
#> 27 Swap more loosely paired functions (paste/paste0, floor/ceiling, ...)
#> 28 Remove a value-preserving wrapper call (rev, unique, as.numeric, ...)
#> 29       Remove tryCatch()/try(), or replace stop() with invisible(NULL)
#> 30                                     Delete a statement of a { } block
#> 31                   Delete a source line (budget: `max_line_deletions`)
#>                                        example
#> 1                             x + y  ->  x - y
#> 2                          x %% y  ->  x %/% y
#> 3                             x < y  ->  x > y
#> 4                            x < y  ->  x <= y
#> 5                           x && y  ->  x || y
#> 6                                    !x  ->  x
#> 7                                a && b  ->  a
#> 8                        if (c)  ->  if (!(c))
#> 9                        if (c)  ->  if (TRUE)
#> 10                             break  ->  next
#> 11                 return(x)  ->  return(NULL)
#> 12                             1  ->  NA_real_
#> 13                            NA  ->  NA_real_
#> 14                                 1  ->  NULL
#> 15                                  10  ->  11
#> 16                                f(x)  ->  42
#> 17                                 "a"  ->  ""
#> 18 f(x, na.rm = TRUE)  ->  f(x, na.rm = FALSE)
#> 19                         x <<- v  ->  x <- v
#> 20                           a && b  ->  a & b
#> 21                   max(a, b)  ->  pmax(a, b)
#> 22                            x[[i]]  ->  x[i]
#> 23               seq_along(x)  ->  1:length(x)
#> 24            m[i, , drop = FALSE]  ->  m[i, ]
#> 25            sum(x, na.rm = TRUE)  ->  sum(x)
#> 26                          any(x)  ->  all(x)
#> 27               paste(a, b)  ->  paste0(a, b)
#> 28                            unique(x)  ->  x
#> 29           tryCatch(f(), error = h)  ->  f()
#> 30                         { a; b }  ->  { b }
#> 31                     <line 3>  ->  <deleted>

# Default set without NA retyping, plus a whole family:
src <- tempfile(fileext = ".R")
writeLines("f <- function(x) if (x > 0) x + 1 else NA", src)
mutants <- mutate_file(src, out_dir = tempfile("mutations_"),
                       operators = c("-na_type_swap", "constants"))
#> Generated 19 AST-based mutants for file1893c79e254.R
```
