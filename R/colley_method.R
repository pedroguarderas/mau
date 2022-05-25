# Colley method ------------------------------------------------------------------------------------
#' @title Colley method
#' @description Rank computation using the Cooley method with ties if required
#' @param n symmetric matrix of number of times each player faced another, zero diagonal
#' @param g total number of games played by each player
#' @param w accumulated vector of wins for each player
#' @param l accumulated vector of losses for each player
#' @param t symmetric matrix of ties, with zero diagonal
#' @return Vector with ratings
#' @author Pedro Guarderas
#' \email{pedro.felipe.guarderas@@gmail.com}
#' @examples
#' # Example without ties
#' n <- matrix( 1, 5, 5 )
#' diag( n ) <- 0
#' g <- rep( 4, 5 )
#' w <- c( 0, 4, 0, 0, 2 )
#' l <- c( 4, 0, 0, 2, 0 )
#' r <- colley_rank( n, g, w, l )
#' @export
colley_rank <- function( n, g, w, l, t = NULL ) {
  
  b <- 1 + 0.5 * ( w - l )
  
  if ( is.null( t ) ) {
    C <- -n
    diag( C ) <- 2 + g
    
  } else {
    c <- t
    c[ lower.tri( c, diag = TRUE ) ] <- 0
    c <- rowSums( c )
    
    C <- -n - t
    diag( C ) <- 2 + g + c
  }

  U <- chol( C )
  U <- chol2inv( U )
  r <- U %*% b
  
  return( r ) 
}
