#___________________________________________________________________________________________________
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
modelo.calcula<-function( file, 
                          utilidades,
                          pesos,
                          tipo = c('RAIZ','CRITERIO', 'SUBCRITERIO', 'UTILIDAD'), 
                          format = 'graphml' ) {
  
  tree<-read.graph( file, format = format )
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
