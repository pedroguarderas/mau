
# Evaluation of decision tree nodes ----------------------------------------------------------------
#' @title Evaluation of decision tree nodes
#' @description Evaluation of decision tree nodes
#' @param tree inicial tree struture with utilities in its leafs
#' @param utilities data.table with ordered columns containing the values of utilities
#' @return data.table with with decision model computed
#' @details Details
#' @author Pedro Guarderas, Andrés Lopez
#' @seealso \code{\link{Stand.String}}, \code{\link{Read.Utilities}}, \code{\link{Eval.Utilities}},
#' \code{\link{Read.Tree}}, \code{\link{Make.Decision.Tree}}.
#' @examples
#' # Index
#' index<-data.table( cod = c( 'A', 'B', 'C', 'D' ), 
#'                    i1 = c( 0.3428570, 1, 1, 1 ),
#'                    i2 = c( 0.5, 0.5, 1, 0.5 ), 
#'                    i3 = c( 0.5, 1.0, 0.75, 0.25 ),
#'                    i4 = c( 0, 0.2696746, 0.6751261, 0.7401660 ),
#'                    i5 = c( 0.2797259, 0.2981198, 1, 0.1952864 ) )
#' 
#' # Loading utilities
#' file<-'example/utilities.txt'
#' script<-'utilities.R'
#' lines<-17
#' skip<-2
#' encoding<-'utf-8'
#' functions<-Read.Utilities( file, script, lines, skip, encoding )
#' source( 'utilities.R' )
#' 
#' # Index positions
#' columns<-c( 2, 3, 5, 6 )
#' 
#' # Associated names of functions
#' functions<-sapply( c( 'Project', 'Self implementation', 'External and local relations', 
#'                       'Scope of capabilities' ),
#'                    FUN = Stand.String )
#' names( functions )<-NULL
#' 
#' # Evaluation of utilities
#' utilities<-Eval.Utilities( index, columns, functions )
#' 
#' # Make tree
#' file<-'example/tree.xls'
#' sheetIndex<-1
#' cols<-1:5
#' rows<-c(1,8)
#' tree.data<-Read.Tree( file, sheetIndex, cols, rows )
#' tree<-Make.Decision.Tree( tree.data )
#' 
#' # Compute the decision model
#' weights<-tree.data[ !is.na( weight ) ]$weight
#' model<-Compute.Model( tree, utilities, weights )
#' @importFrom igraph V
#' @import data.table
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
    
  }
  return( model )
}
