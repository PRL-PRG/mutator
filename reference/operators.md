# Mutation Operators Supported by mutator

This help page lists the mutation operators that the `mutator` package
supports for mutation testing in R.

## Details

The following mutation operators can be applied to R code:

- Arithmetic operators: `+` \<-\> `-`, and `*` \<-\> `/`

- Comparison operators: `==` \<-\> `!=`, `<` \<-\> `>`, and `<=` \<-\>
  `>=`

- Logical operators: `\&` \<-\> `|`, `&&` \<-\> `||`, removes `!`, and
  negates `if` / `while` conditions

- Assignment and call values: replaces assignment right-hand sides and
  ordinary function calls with `42`

- Scalar constants: replaces numeric zero with `42`, numeric non-zero
  values with `0`, constants with a typed `NA`, and constants with
  `NULL`

- Returns: replaces non-constant direct
  [`return()`](https://rdrr.io/r/base/function.html) values with `NULL`,
  for example `return(x)` -\> `return(NULL)`

- Deletions: removes statements inside `{ ... }` blocks and, as a
  fallback, valid source lines

Direct literal return values are not rewritten by the return-to-`NULL`
mutation; for example, `return(1)` is left alone by that mutation.

## Author

Assanali Amandykov and Pierre Donat-Bouillud
