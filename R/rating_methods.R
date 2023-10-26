# Colley method ------------------------------------------------------------------------------------
#' @title Colley method
#' @description Rank computation using the Cooley method with ties if required
#' @param n symmetric matrix of number of times each player faced another, zero diagonal
#' @param w accumulated vector of wins for each player
#' @param l accumulated vector of losses for each player
#' @param t symmetric matrix of ties, with zero diagonal
#' @return Vector with ratings
#' @author Pedro Guarderas
#' \email{pedro.felipe.guarderas@@gmail.com}
#' @examples
#' d <- 10
#' n <- matrix( sample( x = 0:5, size = d * d, replace = TRUE ), d, d )
#' n <- n + t( n )
#' diag( n ) <- 0
#' g <- rowSums( n )
#' # Number of win matches for each team
#' # w <- sapply( 1:d, FUN = function( i ) sample( x = 1:g[i], size = 1, replace = TRUE ) )
#' # Number of lost matches for ech team
#' l <- rowSums( n ) - w
#' r <- colley_rating( n, w, l )
#' @export
colley_rating <- function( n, w, l, t = NULL ) {
  
  b <- 1 + 0.5 * ( w - l )
  g <- rowSums( n )
  
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
  
  r <- solve( C, b )
  v <- rep( 0, length( r ) )
  v[ order( -r ) ] <- 1:length( r )
    
  return( list( v, r ) )
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
#' r <- od_rating( A )
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
#' @param v vector of votes for each ranking
#' @return Vector with aggregated ranking
#' @author Pedro Guarderas
#' \email{pedro.felipe.guarderas@@gmail.com}
#' @examples
#' m <- 10
#' n <- 5
#' R <- matrix( runif( m * n ), m, n )
#' v <- sample( 50:100, n )
#' r <- borda_count( R, v )
#' @export
borda_count <- function( R, v = NULL ) {
  
  m <- nrow( R )
  n <- ncol( R )
  if ( is.null( v ) ) {
    v <- rep( 1, n )
  }
  W <- matrix( 0, m, n )
  
  for ( i in 1:n ) {
    
    r <- R[ , i ]
    w <- rep( 0, m )
    w[ order( -r ) ] <- m:1
    W[ , i ] <- w
    
  }
  
  w <- as.vector( W %*% v )
  r <- rep( 0, m )
  r[ order( -w ) ] <- 1:m
  
  return( list( r, w ) ) 
}
