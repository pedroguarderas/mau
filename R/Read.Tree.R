# Read decision tree -------------------------------------------------------------------------------
#' @title Evaluate utilities
#' @description Read decision tree for MAUT model
#' @param file file
#' @param skip index
#' @param nrows rows
#' @return data.table with utilities
#' @details Details
#' @author Pedro Guarderas, Andrés Lopez
#' @seealso \code{\link{Read.Utilities}}, \code{\link{Make.Decision.Tree}}
#' @examples
#' file<-'example/tree.csv'
#' sheetIndex<-1
#' cols<-1:5
#' rows<-c(1,8)
#' tree.data<-Read.Tree( file, sheetIndex, cols, rows )
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
  
  graph[ !is.na( cod ), funct := name ]
  graph$funct<-sapply( graph$funct, FUN = Stand.String )
  
  graph<-graph[ , list( parent, id, name, cod, weight, name.parent, funct )  ]
  graph<-graph[ with( graph, order( id ) ) ]
  rownames( graph )<-NULL
  
  return( graph )
}
