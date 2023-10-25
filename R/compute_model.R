
# Evaluation of decision tree nodes ----------------------------------------------------------------
#' @title Evaluation of decision tree nodes
#' @description Evaluation of decision tree nodes. All the MAUT model is computed at every level 
#' the utilities are computed considering the given weights.
#' @param tree initial tree structure with utilities in its leafs.
#' @param utilities data.table with ordered columns containing the values of utilities.
#' @param weights weights for the decision model.
#' @return data.table structure containing the utilities of the model for every level the decision
#' tree.
#' @details The whole decision model can be computed a any level and represented in a table format.
#' @author Pedro Guarderas, Andrés Lopez
#' \email{pedro.felipe.guarderas@@gmail.com}
#' @seealso \code{\link{Stand.String}}, \code{\link{Read.Utilities}}, \code{\link{Eval.Utilities}},
#' \code{\link{Read.Tree}}, \code{\link{Make.Decision.Tree}}, \code{\link{Sim.Const.Weights}}.
#' @examples
#' vignette( topic = 'Running_MAUT', package = 'mau' )
#' @import data.table
#' @importFrom igraph make_empty_graph add_vertices add_edges V neighborhood %>%
#' @importFrom stats complete.cases median quantile
#' @export
compute_model <- function( tree, utilities, weights ) {
  
  model <- NULL
  
  criteria <- 1:length( V(tree) )
  
  index <- which( V(tree)$leaf == 1 )
  
  with( utilities, {
    for ( i in criteria ) { # i <- criteria[1]
      
      nl <- unlist( neighborhood( tree, 100, V(tree)[i], mode = 'out' ) )
      
      code <- V(tree)[ nl[ nl %in% index ] ]$code
      
      W <- weights[ code ]
      RW <- W / sum( W )
      uname <- paste( 'u', code, sep = '' )
      
      u <- utilities[ , list( utility = unlist( lapply( .SD * W, sum ) ),
                              relative.utility = unlist( lapply( .SD * RW, sum ) ) ),
                      by = cod, .SDcols = uname ]
      
      u <- u[ , list( utility = sum( utility ),
                      relative.utility = sum( relative.utility ) ), 
              by = cod ]
      
      u[ , id := V(tree)[i]$id ]
      u[ , index := ifelse( V(tree)[i]$code == 0, NA, V(tree)[i]$code ) ]
      u[ , deep := V(tree)[i]$deep ]
      u[ , weight := V(tree)[i]$weight ]
      u[ , relative.weight := V(tree)[i]$rweight ]
      u[ , name := V(tree)[i]$name ]
      u <- u[ , list( id, name, cod, index, deep, utility, relative.utility, weight, relative.weight ) ]
      model <- rbind( model, u )
    }
    return( model )
  })
}

Compute.Model <- function( tree, utilities, weights ) {
  .Deprecated(
    new = 'compute_model',
    msg = 'The function Compute.Model will be replaced by the function compute_model',
    old = 'Compute.Model' )
  return( compute_model( tree, utilities, weights ) )
}
