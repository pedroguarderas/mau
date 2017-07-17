# Simulation of weights ----------------------------------------------------------------------------
#' @title Simulation of weights
#' @description Simulation of weight following a Dirichlet distribution with a concentration 
#' parameter
#' @param n number of simulations
#' @param utilities utility dataframe, first column is the identifier
#' @param alpha concentration parameter for the Dirichlet distribution
#' @return List with data.frames \{simulation, weights\} with total utilities and simulated weights
#' @details Details
#' @author Pedro Guarderas
#' @seealso \code{\link{Eval.Utilities}}
#' @importFrom  gtools rdirichlet
#' @examples
#' N<-10
#' utilities<-data.table( id = 1:N, 
#'                        u1 = runif( N, 0, 1 ), 
#'                        u2 = runif( N, 0, 1 ), 
#'                        u3 = runif( N, 0, 1 ),
#'                        u4 = runif( N, 0, 1 ) )
#' n<-100
#' alpha<-c( 0.2, 0.5, 0.1, 0.2 )
#' S<-Sim.Weights( n, utilities, alpha )
#' @export
Sim.Weights<-function( n, utilities, alpha ) {
  W<-matrix( alpha, length( alpha ), 1 )
  if ( n > 1 ) {
    W<-cbind( W, t( rdirichlet( n = n-1, alpha ) ) )
  }
  colnames( W )<-paste( 'w', 1:n, sep = '' )
  
  M<-ncol( utilities )
  S<-as.matrix( utilities[ , 2:M, with = FALSE ] )
  S<-S %*% W  
  S<-cbind( utilities[,1,with = FALSE], S )
  setnames( S, 1:(n+1), c( 'id', paste( 's', 1:n, sep = '' ) ) )
  return( list( simulation = S, weights = W ) )
}

# Simulation of constrained weights ----------------------------------------------------------------
#' @title Simulation of constrained weights
#' @description Simulation of weight following a Dirichlet distribution with a concentration 
#' parameter and satisfying sum constraints.
#' @param n number of simulations
#' @param utilities utility dataframe, first column is the identifier
#' @param alpha concentration parameter for the Dirichlet distribution
#' @param constraints list of sum constraints
#' @return List with data.frames \{simulation, weights\} with total utilities and simulated weights
#' @details Details
#' @author Pedro Guarderas
#' @seealso \code{\link{Eval.Utilities}}
#' @examples
#' N<-10
#' utilities<-data.table( id = 1:N, 
#'                        u1 = runif( N, 0, 1 ), 
#'                        u2 = runif( N, 0, 1 ), 
#'                        u3 = runif( N, 0, 1 ),
#'                        u4 = runif( N, 0, 1 ) )
#' n<-100
#' alpha<-c( 0.2, 0.5, 0.1, 0.2 )
#' constraints<-list( list( c(1,2), 0.7 ), 
#'                    list( c(3,4), 0.3 ) )
#' S<-Sim.Const.Weights( n, utilities, alpha, constraints )
#' plot.S<-Plot.Simulation.Weight( S$simulation, title = 'Simulations', 
#'                                 xlab = 'ID', ylab = 'Utility' ) 
#' plot( plot.S )
#' @importFrom  gtools rdirichlet
#' @export
Sim.Const.Weights<-function( n, utilities, alpha, constraints ) {

  W<-matrix( 0, length(alpha), n - 1 )
  
  if ( n > 1 ) { 
    for( i in 1:length( constraints ) )  {
      A<-alpha[ constraints[[i]][[1]] ] * constraints[[i]][[2]]
      W[ constraints[[i]][[1]], ]<-constraints[[i]][[2]] * t( rdirichlet( n = n-1, A ) )
    }
  }
  
  W<-cbind( alpha, W )
  colnames( W )<-paste( 'w', 1:n, sep = '' )
  
  M<-ncol( utilities )
  S<-as.matrix( utilities[ , 2:M, with = FALSE ] )
  S<-S %*% W  
  S<-cbind( utilities[,1,with = FALSE], S )
  setnames( S, 1:(n+1), c( 'id', paste( 's', 1:n, sep = '' ) ) )
  
  return( list( simulation = S, weights = W ) )
}
