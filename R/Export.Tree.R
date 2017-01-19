
# Compute decision model for tree ------------------------------------------------------------------
#' @title Compute decision model for tree
#' @description The decision tree for the model base in MAUT has nodes with associated utilities, 
#' this function compute all the internal nodes utilities.
#' @param tree inicial tree struture with utilities in its leafs
#' @param tipo the different names for the levels in the tree, the most common names are: ROOT, 
#' CRITERIA, SUBCRITERIA, UTILITY
#' @return Returns data table with definition of utility functions by range
#' @details Details
#' @author Pedro Guarderas, Andrés Lopez
#' @seealso \code{\link{read_weights}}, \code{\link{eval_index}}
#' @examples
#' x<-2
#' @export
Export.From.Tree<-function( tree, tipo ) {
  
  tabla_objetivos<-data.frame( variable = V(tree)[ V(tree)$leaf == 1 ]$id, 
                               nom = V(tree)[ V(tree)$leaf == 1 ]$name,
                               weight = V(tree)[ V(tree)$leaf == 1 ]$weight,
                               rweight = V(tree)[ V(tree)$leaf == 1 ]$rweight,
                               tipo = tipo[length(tipo)] )
  for ( i in 0:(length(tipo)-2) ) {
    tabla_objetivos<-rbind( tabla_objetivos,
                            data.frame( variable = V(tree)[ V(tree)$deep == i & V(tree)$leaf == 0 ]$id, 
                                        nom = V(tree)[ V(tree)$deep == i & V(tree)$leaf == 0 ]$name,
                                        weight = V(tree)[ V(tree)$deep == i & V(tree)$leaf == 0 ]$weight,
                                        rweight = V(tree)[ V(tree)$deep == i & V(tree)$leaf == 0 ]$rweight,
                                        tipo = tipo[i+1] )
    )
  }
  return( tabla_objetivos )
}

# Compute decision model for tree ------------------------------------------------------------------
#' @title Compute decision model for tree
#' @description The decision tree for the model base in MAUT has nodes with associated utilities, 
#' this function compute all the internal nodes utilities.
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
#' @export
Export.Decision.Tree<-function( tree, 
                                utilities,
                                weights,
                                types = c('root','criteria', 'subcriteria', 'utility') ) {
  
  Obj<-modelo.calcula.tree( tree, types )
  
  model<-eval_criteria( tree, weights, utilities, FALSE, TRUE, TRUE )
  model.resc<-eval_criteria( tree, weights, utilities, TRUE, TRUE, TRUE )
  
  MT<-melt( model, id.vars = 'cod' )
  MTR<-melt( model.resc, id.vars = 'cod' )
  names( MTR )<-c('cod','variable','val_res')
  MT<-merge( MT, MTR, by = c('cod','variable') )
  MT<-merge( MT, Obj, by.x = 'variable', by.y = 'variable' )
  MT<-MT[ with( MT, order( variable, cod ) ),  ]
  rownames( MT )<-NULL
  
  return( MT)
}
