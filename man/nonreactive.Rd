\name{nonreactive}
\alias{nonreactive}
\title{Create a non-reactive scope for an expression}
\usage{
  nonreactive(expr)
}
\arguments{
  \item{expr}{An expression that can access reactive values
  or functions.}
}
\description{
  Takes an expression and creates a new context for
  evaluating that expression which can receive
  invalidations but does not in turn invalidate other
  contexts. This is useful when you want a function to
  access reactive values or functions but don't want
  changes to those values to trigger re-evaluation of the
  function.
}

