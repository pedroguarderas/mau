# Simulation of weights ----------------------------------------------------------------------------
#' @title Simulation of weights
#' @description Simulation of weights employing the Dirichlet distribution. The concentration 
#' parameters for the Dirichlet distribution are tentative weights.
#' @param n number of simulations
#' @param utilities utility dataframe, first column is the identifier
#' @param alpha concentration parameter for the Dirichlet distribution
#' @return List with data.frames \{simulation, weights\} with total utilities and simulated weights
#' @details Taking advantage of the Dirichlet distribution properties, the weights could be 
#' simulated with a concentration around given weights.
#' @author Pedro Guarderas
#' \email{pedro.felipe.guarderas@@gmail.com}
#' @seealso \code{\link{eval_utilities}}
#' @importFrom gtools rdirichlet
#' @examples
#' library( data.table )
#' N <- 10
#' utilities <- data.table( id = 1:N, 
#'                        u1 = runif( N, 0, 1 ), 
#'                        u2 = runif( N, 0, 1 ), 
#'                        u3 = runif( N, 0, 1 ),
#'                        u4 = runif( N, 0, 1 ) )
#' n <- 100
#' alpha <- c( 0.2, 0.5, 0.1, 0.2 )
#' S <- sim_weights( n, utilities, alpha )
#' @export
sim_weights <- function( n, utilities, alpha ) {
  W <- matrix( alpha, length( alpha ), 1 )
  if ( n > 1 ) {
    W <- cbind( W, t( rdirichlet( n = n-1, alpha ) ) )
  }
  colnames( W ) <- paste( 'w', 1:n, sep = '' )
  
  M <- ncol( utilities )
  S <- as.matrix( utilities[ , 2:M, with = FALSE ] )
  S <- S %*% W  
  S <- cbind( utilities[,1,with = FALSE], S )
  setnames( S, 1:(n+1), c( 'id', paste( 's', 1:n, sep = '' ) ) )
  return( list( simulation = S, weights = W ) )
}

Sim.Weights <- function( n, utilities, alpha ) {
  .Deprecated(
    new = 'sim_weights',
    msg = 'The function Sim.Weights will be replaced by the function sim_weights',
    old = 'Sim.Weights' )
  return( sim_weights( n, utilities, alpha ) )
}

# Simulation of constrained weights ----------------------------------------------------------------
#' @title Simulation of constrained weights
#' @description Simulation of weights employing the Dirichlet distribution. The concentration 
#' parameters for the Dirichlet distribution are tentative weights, additionally constraints over
#' partial sums of weights are introduced by a list ordered structure.
#' @param n number of simulations
#' @param utilities utility dataframe, first column is the identifier
#' @param alpha concentration parameter for the Dirichlet distribution
#' @param constraints list of sum constraints
#' @return List with data.frames \{simulation, weights\} with total utilities and simulated weights
#' @details Employing the properties of the Dirichlet distribution, weights could be simulated 
#' with a given concentration, additionally this simulation can be carry out by subsets of weights
#' only to meet specific constraints.
#' @author Pedro Guarderas
#' \email{pedro.felipe.guarderas@@gmail.com}
#' @seealso \code{\link{eval_utilities}}
#' @examples
#' library( data.table )
#' N <- 10
#' utilities <- data.table( id = 1:N, 
#'                        u1 = runif( N, 0, 1 ), 
#'                        u2 = runif( N, 0, 1 ), 
#'                        u3 = runif( N, 0, 1 ),
#'                        u4 = runif( N, 0, 1 ) )
#' n <- 100
#' alpha <- c( 0.2, 0.5, 0.1, 0.2 )
#' constraints <- list( list( c(1,2), 0.7 ), 
#'                    list( c(3,4), 0.3 ) )
#' S <- sim_const_weights( n, utilities, alpha, constraints )
#' plot.S <- plot_sim_weight( S$simulation, title = 'Simulations', 
#'                            xlab = 'ID', ylab = 'Utility' ) 
#' plot( plot.S )
#' @importFrom gtools rdirichlet
#' @export
sim_const_weights <- function( n, utilities, alpha, constraints ) {
  
  W <- matrix( 0, length(alpha), n - 1 )
  
  if ( n > 1 ) { 
    for( i in 1:length( constraints ) )  {
      A <- alpha[ constraints[[i]][[1]] ] * constraints[[i]][[2]]
      W[ constraints[[i]][[1]], ] <- constraints[[i]][[2]] * t( rdirichlet( n = n-1, A ) )
    }
  }
  
  W <- cbind( alpha, W )
  colnames( W ) <- paste( 'w', 1:n, sep = '' )
  
  M <- ncol( utilities )
  S <- as.matrix( utilities[ , 2:M, with = FALSE ] )
  S <- S %*% W  
  S <- cbind( utilities[,1,with = FALSE], S )
  setnames( S, 1:(n+1), c( 'id', paste( 's', 1:n, sep = '' ) ) )
  
  return( list( simulation = S, weights = W ) )
}

Sim.Const.Weights <- function( n, utilities, alpha, constraints ) {
  .Deprecated(
    new = 'sim_const_weights',
    msg = 'The function Sim.Const.Weights will be replaced by the function sim_const_weights',
    old = 'Sim.Const.Weights' )
  return( sim_const_weights( n, utilities, alpha, constraints ) )
}