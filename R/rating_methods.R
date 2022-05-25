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
#' r <- colley_rating( n, g, w, l )
#' @export
colley_rating <- function( n, g, w, l, t = NULL ) {
  
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
  
  return( list( r = o / d, o = o, d = d, iter = k ) )
}

# Borda count rank aggregation ---------------------------------------------------------------------
#' @title Borda count
#' @description Rank aggregation with the Borda count method
#' @param R matrix with rankings
#' @return Vector with aggregated ranking
#' @author Pedro Guarderas
#' \email{pedro.felipe.guarderas@@gmail.com}
#' @examples
#' @export
borda_count <- function( R ) {
  n <- nrow( R )
  b <- rowSums( apply( R, 2, FUN = function( r ) n - order( -r ) ) )
  return( b ) 
}
