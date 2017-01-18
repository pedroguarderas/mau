
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
#' @export
Read.Tree<-function( file, index_file, sheet, cols, rows, cols_index ) {
  
  graph<-read.xlsx( file = file, sheetIndex = sheet, startRow = rows[1], endRow = rows[2],
                    colIndex = cols, colClasses = c('integer','character', 'integer', 'integer' ) )
  colnames( graph )<-c('id','nom','parent','cod')
  graph<-merge( graph, subset( graph, select = c('id','nom') ),
                by.x = 'parent', by.y = 'id', all.x = TRUE )
  colnames( graph )<-c( 'id_parent', 'id', 'nom', 'cod', 'parent' )
  
  index<-index_file[,cols_index]
  colnames( index )<-c('cod','function','weight')
  graph<-merge( graph, index, by = 'cod', all.x = TRUE  )
  graph<-graph[ ,c( 'id', 'id_parent', 'cod', 'nom', 'parent', 'function', 'weight' ) ]
  graph<-graph[ order( graph$id ), ]
  
  rm( sheet, cols, rows, cols_index )
  rownames( graph )<-NULL
  return( graph )
}
