# Evaluate utility functions -----------------------------------------------------------------------
#' @title Evaluate utilities
#' @description Evaluation of utilities for a data.table of indexes, the utilities functions are 
#' computed over every index represented by each column of the input table.
#' @param index data.table of indexes.
#' @param columns columns with indexes where the utilities will be computed.
#' @param functions vector of characters with name of functions.
#' @return data.table with utilities evaluated for every index.
#' @details Every index has associated an utility function, inside \code{mau} is possible to employ
#' any functions, the only special requirement is that the utility has to be normalized, this means
#' that the utility is bounded between 0 and 1.
#' 
#' Also is possible to consider utilities with constant risk aversion CRA, in the sense of Arrow, 
#' for such case there is only two types of functions \eqn{u(x) = a x + b} or 
#' \eqn{u(x) = a e^{bx} + c}, to determine these functions, it is only necessary to specify the 
#' parameters \eqn{a}, \eqn{b} and \eqn{c}. For a decision model only elaborated with CRA 
#' utilities, \code{mau} could read a text file where every utility is piecewise defined.
#' 
#' The format for the text file containing the definition of utility functions is given by is:
#' 
#'   [Header] \cr
#' \cr
#'   [Function name] \cr
#'   [min1 max1 a1 b1 c1] \cr
#'   [min2 max2 a2 b2 c2] \cr
#'   [min3 max3 a3 b3 c3] \cr
#' ... \cr
#'   [Function name] \cr
#'   [min1 max1 a1 b1 c1] \cr
#'   [min2 max2 a2 b2 c2] \cr
#'   [min3 max3 a3 b3 c3] \cr
#' ... \cr
#' 
#' If the coefficient c is non zero the function is interpreted as an exponential type.
#' 
#' @author Pedro Guarderas, \email{pedro.felipe.guarderas@@gmail.com}, Andrés Lopez.
#' @seealso \code{\link{Read.Utilities}}, \code{\link{Stand.String}}
#' @examples
#' library( mau )
#' vignette( topic = 'Running_MAUT', package = 'mau' ) 
#' @export
eval_utilities <- function( index, columns, functions ) {
  
  utilities <- copy( index )
  
  if ( length( columns ) == length( functions ) ) {
    N <- length( columns )
    for ( i in 1:N ) {
      c <- names( utilities )[ columns[i] ]
      c <- as.name( c )
      f <- functions[i]
      f <- as.name( f )
      E <- substitute( expression( utilities$column <- sapply( utilities$column, FUN = Function  ) ), 
                     list( column = c, Function = f ) )
      E <- eval( E )
      eval( E )
    }
  }
  
  setnames( utilities, 1:ncol( utilities ), 
            c( names( utilities )[1], paste( 'u', 1:( ncol(utilities) - 1 ), sep = '' ) ) )

  return( utilities )
}

Eval.Utilities <- function( index, columns, functions ) {
  .Deprecated(
    new = 'eval_utilities',
    msg = 'The function Eval.Utilities will be replaced by the function eval_utilities',
    old = 'Eval.Utilities' )
  return( eval_utilities( index, columns, functions ) )
}
