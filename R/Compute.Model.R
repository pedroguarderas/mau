
# Evaluation of decision tree nodes ----------------------------------------------------------------
#' @title Evaluation of decision tree nodes
#' @description Evaluation of decision tree nodes. All the MAUT model is computed at every level 
#' the utilities are computed considering the given weights.
#' @param tree inicial tree struture with utilities in its leafs
#' @param utilities data.table with ordered columns containing the values of utilities
#' @param weights decision model weights
#' @return data.table with with decision model computed
#' @details Details
#' @author Pedro Guarderas, Andrés Lopez
#' @seealso \code{\link{Stand.String}}, \code{\link{Read.Utilities}}, \code{\link{Eval.Utilities}},
#' \code{\link{Read.Tree}}, \code{\link{Make.Decision.Tree}}.
#' @examples
#' # Index
#' library( data.table )
#' library( igraph )
#' library( ggplot2 )
#' 
#' index<-data.table( cod = paste( 'A', 1:10, sep = '' ), 
#'                    i1 = c( 0.34, 1, 1, 1, 1, 0.2, 0.7, 0.5, 0.11, 0.8 ),
#'                    i2 = c( 0.5, 0.5, 1, 0.5, 0.3, 0.1, 0.4, 0.13, 1, 0.74 ), 
#'                    i3 = c( 0.5, 1.0, 0.75, 0.25, 0.1, 0.38, 0.57, 0.97, 0.3, 0.76 ),
#'                    i4 = c( 0, 0.26, 0.67, 0.74, 0.84, 0.85, 0.74, 0.65, 0.37, 0.92 ) )
#' # Loading utilities
#' file<-system.file("extdata", "utilities.txt", package = "mau" )
#' script<-'utilities.R'
#' lines<-17
#' skip<-2
#' encoding<-'utf-8'
#' functions<-Read.Utilities( file, script, lines, skip, encoding )
#' source( 'utilities.R' )
#' 
#' # Index positions
#' columns<-c( 2, 3, 4, 5 )
#' 
#' # Associated names of functions
#' functions<-sapply( c( 'Project', 
#'                       'Self implementation',
#'                       'External and local relations', 
#'                       'Scope of capabilities' ),
#'                    FUN = Stand.String )
#' names( functions )<-NULL
#' 
#' # Evaluation of utilities
#' utilities<-Eval.Utilities( index, columns, functions )
#' 
#' # Make tree
#' file<-system.file("extdata", "tree.csv", package = "mau" )
#' sheetIndex<-1
#' cols<-1:5
#' rows<-c(1,8)
#' tree.data<-Read.Tree( file, skip = 0, nrow = 8 )
#' tree<-Make.Decision.Tree( tree.data )
#' 
#' # Compute the decision model
#' weights<-tree.data[ !is.na( weight ) ]$weight
#' model<-Compute.Model( tree, utilities, weights )
#' 
#' # Simulation of weights
#' n<-200
#' alpha<-c( 0.2, 0.5, 0.1, 0.2 )
#' constraints<-list( list( c(1,2), 0.7 ), 
#'                    list( c(3,4), 0.3 ) )
#' S<-Sim.Const.Weights( n, utilities, alpha, constraints )
#' plot.S<-Plot.Simulation.Weight( S$simulation, title = 'Simulations', 
#'                                 xlab = 'ID', ylab = 'Utility' ) 
#' plot( plot.S )
#' @import data.table
#' @importFrom igraph make_empty_graph add_vertices add_edges V neighborhood %>%
#' @importFrom stats complete.cases median quantile
#' @export
Compute.Model<-function( tree, 
                         utilities, 
                         weights ) {
  
  model<-NULL
  
  criteria<-1:length( V(tree)$leaf )
  
  index<-which( V(tree)$leaf == 1 )
  
  for ( i in criteria ) { # i<-criteria[1]
    
    nl<-unlist( neighborhood( tree, 100, V(tree)[i], mode = 'out' ) )
    
    code<-V(tree)[ nl[ nl %in% index ] ]$code
    
    W<-weights[ code ]
    RW<-W / sum( W )
    uname<-paste( 'u', code, sep = '' )
    
    with( utilities, {
      u<-utilities[ , list( utility = unlist( lapply( .SD * W, sum ) ),
                            rutility = unlist( lapply( .SD * RW, sum ) ) ),
                    by = cod, .SDcols = uname ]
  
      u<-u[ , list( utility = sum( utility ),
                    rutility = sum( rutility ) ), 
            by = cod ]
      
      u[ , id := V(tree)[i]$id ]
      u[ , code := ifelse( V(tree)[i]$code == 0, NA, V(tree)[i]$code ) ]
      u[ , weight := V(tree)[i]$weight ]
      u[ , rweight := V(tree)[i]$rweight ]
      u[ , name := V(tree)[i]$name ]
      u<-u[ , list( id, name, code, cod, utility, rutility, weight, rweight ) ]
      model<-rbind( model, u )
    })
  }
  return( model )
}
