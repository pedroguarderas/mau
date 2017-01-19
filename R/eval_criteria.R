
# Evaluation of decision tree nodes ----------------------------------------------------------------
#' @title Evaluation of decision tree nodes
#' @description Evaluation of decision tree nodes
#' @param tree inicial tree struture with utilities in its leafs
#' @param utilities inicial tree struture with utilities in its leafs
#' @param weights weights
#' @param types the different names for the levels in the tree, the most common names are: 'root',
#' 'criteria', 'subcriteria', 'utility'
#' @return data.table with with decision model computed
#' @details Details
#' @author Pedro Guarderas, Andrés Lopez
#' @seealso \code{\link{read_weights}}, \code{\link{eval_index}}
#' @examples
#' x<-2
#' @importFrom igraph V
#' @export
eval_criteria<-function( tree, weights, utilidades, rescale = FALSE, indexto = TRUE, idcolname = TRUE ) {
  model<-data.frame( cod = utilidades$cod )
  if ( indexto ) {
    criteria<-1:length(V(tree)$leaf)
  } else {
    criteria<-which( V(tree)$leaf == 0 )
  }
  
  index<-which( V(tree)$leaf == 1 )
  
  for ( i in criteria ) { # i<-criteria[2]
    nl<-unlist( neighborhood( tree, 100, V(tree)[i], mode = 'out' ) )
    index_cod<-V(tree)[ nl[ nl %in% index ] ]$codigo
    col_eva<-paste( 'eva_', index_cod, sep = '' )
    w<-weights[ index_cod ]
    val<-as.matrix( utilidades[ ,col_eva ] )
    if ( rescale ) {
      model<-cbind( model, val %*% w / sum(w) )
    } else {
      model<-cbind( model, val %*% w )
    }
    if ( idcolname ) {
      colnames( model )[ncol(model)]<-V(tree)[i]$id
    } else {
      colnames( model )[ncol(model)]<-V(tree)[i]$name
    }
    
  }
  return( model )
}
