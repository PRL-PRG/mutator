# Mutation Operators Supported by mutator

This help page lists the mutation operators that the `mutator` package
supports for mutation testing in R.

## Details

The following mutation operators can be applied to R code:

- `cxx_add_to_sub`: Replaces `+` with `-`

- `cxx_and_to_or`: Replaces `\&` with `|`

- `cxx_assign_const`: Replaces `a = b` with `a = 42`

- `cxx_div_to_mul`: Replaces `/` with `*`

- `cxx_eq_to_ne`: Replaces `==` with `!=`

- `cxx_ge_to_gt`: Replaces `>=` with `>`

- `cxx_ge_to_lt`: Replaces `>=` with `<`

- `cxx_gt_to_ge`: Replaces `>` with `>=`

- `cxx_gt_to_le`: Replaces `>` with `<=`

- `cxx_le_to_gt`: Replaces `<=` with `>`

- `cxx_le_to_lt`: Replaces `<=` with `<`

- `cxx_logical_and_to_or`: Replaces `&&` with `||`

- `cxx_logical_or_to_and`: Replaces `||` with `&&`

- `cxx_lt_to_ge`: Replaces `<` with `>=`

- `cxx_lt_to_le`: Replaces `<` with `<=`

- `cxx_minus_to_noop`: Replaces `-x` with `x`

- `cxx_mul_to_div`: Replaces `*` with `/`

- `cxx_ne_to_eq`: Replaces `!=` with `==`

- `cxx_or_to_and`: Replaces `|` with `\&`

- `cxx_remove_negation`: Replaces `!a` with `a`

- `cxx_replace_scalar_call`: Replaces a function call with `42`

- `cxx_sub_to_add`: Replaces `-` with `+`

- `negate_mutator`: Negates conditionals `!x` to `x` and `x` to `!x`

- `scalar_value_mutator`: Replaces zeros with `42`, and non-zeros with
  `0`

These operators allow the `mutator` package to systematically alter
source code in controlled ways, enabling thorough testing of R code and
ensuring that tests are sensitive to subtle code changes.

## Author

Assanali Amandykov and Pierre Donat-Bouillud
