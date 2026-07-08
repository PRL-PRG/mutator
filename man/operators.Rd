\name{operators}
\alias{operators}
\title{Mutation Operators Supported by mutator}
\description{
This help page lists the mutation operators that the \code{mutator} package supports for mutation testing in R.
}
\details{
The following mutation operators can be applied to R code:

\itemize{
  \item Arithmetic operators: \code{+} <-> \code{-}, and \code{*} <-> \code{/}
  \item Comparison operators: \code{==} <-> \code{!=}, \code{<} <-> \code{>}, and \code{<=} <-> \code{>=}
  \item Logical operators: \code{\&} <-> \code{|}, \code{&&} <-> \code{||}, removes \code{!}, and negates \code{if} / \code{while} conditions
  \item Assignment and call values: replaces assignment right-hand sides and ordinary function calls with \code{42}
  \item Scalar constants: replaces numeric zero with \code{42}, numeric non-zero values with \code{0}, constants with a typed \code{NA}, and constants with \code{NULL}
  \item Returns: replaces non-constant direct \code{return()} values with \code{NULL}, for example \code{return(x)} -> \code{return(NULL)}
  \item Deletions: removes statements inside \code{\{ ... \}} blocks and, as a fallback, valid source lines
}

Direct literal return values are not rewritten by the return-to-\code{NULL} mutation; for example, \code{return(1)} is left alone by that mutation.
}
\author{
Assanali Amandykov and Pierre Donat-Bouillud
}
