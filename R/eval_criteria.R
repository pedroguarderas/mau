#___________________________________________________________________________________________________
# Función para evaluación de criterios y subcriterios

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