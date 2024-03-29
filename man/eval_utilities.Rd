% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/eval_utilities.R
\name{eval_utilities}
\alias{eval_utilities}
\title{Evaluate utilities}
\usage{
eval_utilities(index, columns, functions)
}
\arguments{
\item{index}{data.table of indexes.}

\item{columns}{columns with indexes where the utilities will be computed.}

\item{functions}{vector of characters with name of functions.}
}
\value{
data.table with utilities evaluated for every index.
}
\description{
Evaluation of utilities for a data.table of indexes, the utilities functions are 
computed over every index represented by each column of the input table.
}
\details{
Every index has associated an utility function, inside \code{mau} is possible to employ
any functions, the only special requirement is that the utility has to be normalized, this means
that the utility is bounded between 0 and 1.

Also is possible to consider utilities with constant risk aversion CRA, in the sense of Arrow, 
for such case there is only two types of functions \eqn{u(x) = a x + b} or 
\eqn{u(x) = a e^{bx} + c}, to determine these functions, it is only necessary to specify the 
parameters \eqn{a}, \eqn{b} and \eqn{c}. For a decision model only elaborated with CRA 
utilities, \code{mau} could read a text file where every utility is piecewise defined.

The format for the text file containing the definition of utility functions is given by is:

  [Header] \cr
\cr
  [Function name] \cr
  [min1 max1 a1 b1 c1] \cr
  [min2 max2 a2 b2 c2] \cr
  [min3 max3 a3 b3 c3] \cr
... \cr
  [Function name] \cr
  [min1 max1 a1 b1 c1] \cr
  [min2 max2 a2 b2 c2] \cr
  [min3 max3 a3 b3 c3] \cr
... \cr

If the coefficient c is non zero the function is interpreted as an exponential type.
}
\examples{
library( mau )
vignette( topic = 'Running_MAUT', package = 'mau' ) 
}
\seealso{
\code{\link{read_utilities}}, \code{\link{stand_string}}
}
\author{
Pedro Guarderas, \email{pedro.felipe.guarderas@gmail.com}, Andrés Lopez.
}
