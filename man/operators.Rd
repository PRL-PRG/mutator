\name{myfunctions}
\alias{myfunctions}
\title{Mutation Operators Supported by mutator}
\description{
This help page lists the mutation operators that the \code{mutator} package supports for mutation testing in R.
}
\details{
The following mutation operators can be applied to R code:

\itemize{
  \item \code{cxx_add_to_sub}: Replaces \code{+} with \code{-}
  \item \code{cxx_and_to_or}: Replaces \code{\&} with \code{|}
  \item \code{cxx_assign_const}: Replaces \code{a = b} with \code{a = 42}
  \item \code{cxx_div_to_mul}: Replaces \code{/} with \code{*}
  \item \code{cxx_eq_to_ne}: Replaces \code{==} with \code{!=}
  \item \code{cxx_ge_to_gt}: Replaces \code{>=} with \code{>}
  \item \code{cxx_ge_to_lt}: Replaces \code{>=} with \code{<}
  \item \code{cxx_gt_to_ge}: Replaces \code{>} with \code{>=}
  \item \code{cxx_gt_to_le}: Replaces \code{>} with \code{<=}
  \item \code{cxx_le_to_gt}: Replaces \code{<=} with \code{>}
  \item \code{cxx_le_to_lt}: Replaces \code{<=} with \code{<}
  \item \code{cxx_logical_and_to_or}: Replaces \code{&&} with \code{||}
  \item \code{cxx_logical_or_to_and}: Replaces \code{||} with \code{&&}
  \item \code{cxx_lt_to_ge}: Replaces \code{<} with \code{>=}
  \item \code{cxx_lt_to_le}: Replaces \code{<} with \code{<=}
  \item \code{cxx_minus_to_noop}: Replaces \code{-x} with \code{x}
  \item \code{cxx_mul_to_div}: Replaces \code{*} with \code{/}
  \item \code{cxx_ne_to_eq}: Replaces \code{!=} with \code{==}
  \item \code{cxx_or_to_and}: Replaces \code{|} with \code{\&}
  \item \code{cxx_remove_negation}: Replaces \code{!a} with \code{a}
  \item \code{cxx_replace_scalar_call}: Replaces a function call with \code{42}
  \item \code{cxx_sub_to_add}: Replaces \code{-} with \code{+}
  \item \code{negate_mutator}: Negates conditionals \code{!x} to \code{x} and \code{x} to \code{!x}
  \item \code{scalar_value_mutator}: Replaces zeros with \code{42}, and non-zeros with \code{0}
}

These operators allow the \code{mutator} package to systematically alter source code in controlled ways, enabling thorough testing of R code and ensuring that tests are sensitive to subtle code changes.
}
\author{
Assanali Amandykov and Pierre Donat-Bouillud
}
