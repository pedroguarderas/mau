# Offensive - Defensive rating method --------------------------------------------------------------
#' @title Offensive - Defensive rating method
#' @description Computes rating using the offensive-defensive method
#' @param A matrix with scores
#' @param iter number of iterations
#' @param rer relative error to stop computation
#' @return list with rating, offensive score, defensive score and number of iterations.
#' @author Pedro Guarderas
#' \email{pedro.felipe.guarderas@@gmail.com}
#' @examples
#' A <- matrix( c( 0, 7, 21, 7, 0,
#' 52, 0, 34, 25, 27, 
#' 24, 16, 0, 7, 3,
#' 38, 17, 5, 0, 14,
#' 45, 7, 30, 52, 0 ), nrow = 5, ncol = 5 )
#' od_rating( A ) 
#' @export
od_rating <- function( A, iter = 1000, rer = 1e-12 ) {
  m <- nrow( A )
  
  d <- rep( 1, m )
  o <- d
  err <- 2 * rer
  
  k <- 1
  while ( err > rer & k <= iter ) {
    o0 <- o
    d0 <- d
    
    o <- t( A ) %*% ( 1 / d0 )
    d <- A %*% ( 1 / o )
    
    k <- k + 1
    
    err <- norm( o - o0, type = '2' ) / norm( o0, type = '2' )
  }
  
  return( list( r = o / d, o = o, d = d, k ) )
}
