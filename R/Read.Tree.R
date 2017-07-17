# Read decision tree -------------------------------------------------------------------------------
#' @title Evaluate utilities
#' @description The tree structure for the MAUT is extracted from a file with a given format.
#' @param file input file
#' @param skip starting row for read
#' @param nrows number of rows to be readed
#' @return data.table with utilities
#' @author Pedro Guarderas, Andrés Lopez
#' @seealso \code{\link{Read.Utilities}}, \code{\link{Make.Decision.Tree}}
#' @examples
#' library( data.table )
#' library( igraph )
#' file<-system.file("extdata", "tree.csv", package = "mau" )
#' sheetIndex<-1
#' tree.data<-Read.Tree( file, skip = 0, nrows = 8 )
#' @importFrom utils read.csv
#' @importFrom data.table setnames as.data.table
#' @export
Read.Tree<-function( file, 
                     skip, 
                     nrows ) {
  
  graph<-read.csv( file, header = TRUE, sep = ",", quote = "\"'", skip = skip, nrows = nrows,
                    colClasses = c('integer','character', 'integer', 'integer', 'numeric' ),
                    stringsAsFactors = FALSE )
  
  graph<-as.data.table( graph )
  setnames( graph, 1:5, c('id','name','parent','cod','weight') )
  
  graph<-merge( graph, 
                subset( graph, select = c( 'id','name' ) ),
                by.x = 'parent', by.y = 'id', all.x = TRUE )
  
  setnames( graph, 1:6, c( 'parent', 'id', 'name', 'cod', 'weight', 'name.parent' ) )
  
  with( graph, {
  graph[ !is.na( cod ), funct := name ]
  graph$funct<-sapply( graph$funct, FUN = Stand.String )
  
  graph<-graph[ , list( parent, id, name, cod, weight, name.parent, funct )  ]
  graph<-graph[ with( graph, order( id ) ) ]
  rownames( graph )<-NULL

  return( graph )
  })
}
