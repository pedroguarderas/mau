
# Evaluate utility functions -----------------------------------------------------------------------
#' @title Evaluate utilities
#' @description Evaluaction of utilities for a data.table of indexes
#' @param index data.table of indexes
#' @param columns columns with indexes where the utilities will be computed
#' @param functions vector of characters with name of functions
#' @return data.table with utilities
#' @details Details
#' @author Pedro Guarderas, Andrés Lopez
#' @seealso \code{\link{Read.Utilities}}, \code{\link{Stand.String}}
#' @examples
#' # Index
#' index<-data.table( cod = c( 'A', 'B', 'C', 'D' ), 
#'                    i1 = c( 0.3428570, 1, 1, 1 ),
#'                    i2 = c( 0.5, 0.5, 1, 0.5 ), 
#'                    i3 = c( 0.5, 1.0, 0.75, 0.25 ),
#'                    i4 = c( 0, 0.2696746, 0.6751261, 0.7401660 ),
#'                    i5 = c( 0.2797259, 0.2981198, 1, 0.1952864 ) )
#' 
#' # Index name
#' columns<-c( 2, 3, 5, 6 )
#' functions<-sapply( c( 'Project', 'Self implementation', 'External and local relations', 
#'                       'Scope of capabilities' ),
#'                    FUN = Stand.String )
#' names( functions )<-NULL
#' 
#' # Loading utilities
#' source( 'utilities.R' )
#'  
#' # Evaluation of utilities
#' utilities<-Eval.Utilities( index, columns, functions )
#' @export
Eval.Utilities<-function( index, columns, functions ) {
  
  utilities<-copy( index )
  
  if ( length( columns ) == length( functions ) ) {
    N<-length( columns )
    for ( i in 1:N ) {
      c<-names( utilities )[ columns[i] ]
      c<-as.name( c )
      f<-functions[i]
      f<-as.name( f )
      E<-substitute( expression( utilities$column<-sapply( utilities$column, FUN = Function  ) ), 
                     list( column = c, Function = f ) )
      E<-eval( E )
      eval( E )
    }
  }

  return( utilities )
}
