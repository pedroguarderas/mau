
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
modelo.calcula.tree<-function( tree, tipo ) {
  
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

#___________________________________________________________________________________________________
modelo.calcula<-function( tree, 
                          utilidades,
                          pesos,
                          tipo = c('RAIZ','CRITERIO', 'SUBCRITERIO', 'UTILIDAD') ) {
  
  tabla_objetivos<-modelo.calcula.tree( tree, tipo )
  
  modelo<-eval_criteria( tree, pesos, utilidades, FALSE, TRUE, TRUE )
  modelo.resc<-eval_criteria( tree, pesos, utilidades, TRUE, TRUE, TRUE )
  
  modelo_tabla<-melt( modelo, id.vars = 'cod' )
  modelo_tabla_rescaled<-melt( modelo.resc, id.vars = 'cod' )
  names( modelo_tabla_rescaled )<-c('cod','variable','val_res')
  modelo_tabla<-merge( modelo_tabla, modelo_tabla_rescaled, by = c('cod','variable') )
  modelo_tabla<-merge( modelo_tabla, tabla_objetivos, by.x = 'variable', by.y = 'variable' )
  modelo_tabla<-modelo_tabla[ with( modelo_tabla, order( variable, cod ) ),  ]
  rownames( modelo_tabla )<-NULL
  
  return( modelo_tabla)
}
