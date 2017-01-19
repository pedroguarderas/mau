
# Read decision tree -------------------------------------------------------------------------------
#' @title Evaluate utilities
#' @description Read decision tree for MAUT model
#' @param file file
#' @param index_file index
#' @param sheet sheet
#' @param cols cols
#' @param rows rows
#' @param cols_index cols_index
#' @return data.table with utilities
#' @details Details
#' @author Pedro Guarderas, Andrés Lopez
#' @seealso \code{\link{Read.Utilities}}, \code{\link{Make.Decision.Tree}}
#' @examples
#' Read.Tree( file, index_file, sheet, cols, rows, cols_index )
#' @importFrom xlsx read.xlsx
#' @export
Read.Tree<-function( file, 
                     sheetIndex, 
                     cols, 
                     rows ) {
  
  graph<-read.xlsx( file, sheetIndex, 
                    startRow = rows[1], 
                    endRow = rows[2],
                    colIndex = cols, 
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
