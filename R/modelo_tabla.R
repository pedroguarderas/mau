modelo_tabla<- function(tree, utilidades){
  
  model<-eval_criteria( tree, W[,1], utilidades, FALSE, TRUE, TRUE )
  model_rescale<-eval_criteria( tree, W[,1], utilidades, TRUE, TRUE, TRUE )
  
  tip<-c('RAIZ','CRITERIO','SUBCRITERIO','UTILIDAD')
  
  tabla_objetivos<-data.frame( variable = V(tree)[ V(tree)$leaf == 1 ]$id, 
                             nom = V(tree)[ V(tree)$leaf == 1 ]$name,
                             weight = V(tree)[ V(tree)$leaf == 1 ]$weight,
                             rweight = V(tree)[ V(tree)$leaf == 1 ]$rweight,
                             tipo = tip[4] )
  
  for ( i in 0:(length(tip)-2) ) {
  tabla_objetivos<-rbind( tabla_objetivos,
                          data.frame( variable = V(tree)[ V(tree)$deep == i & V(tree)$leaf == 0 ]$id, 
                                      nom = V(tree)[ V(tree)$deep == i & V(tree)$leaf == 0 ]$name,
                                      weight = V(tree)[ V(tree)$deep == i & V(tree)$leaf == 0 ]$weight,
                                      rweight = V(tree)[ V(tree)$deep == i & V(tree)$leaf == 0 ]$rweight,
                                      tipo = tip[i+1] )
  )
  }
  
  modelo_tabla<-melt( model, id.vars = 'cod')
  modelo_tabla_rescaled<-melt( model_rescale, id.vars = 'cod')
  names( modelo_tabla_rescaled )<-c('cod','variable','val_res')
  modelo_tabla<-merge( modelo_tabla, modelo_tabla_rescaled, by = c('cod','variable') )
  modelo_tabla<-merge( modelo_tabla, tabla_objetivos, by.x = 'variable', by.y = 'variable' )

  return(modelo_tabla)
}

