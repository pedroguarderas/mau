#___________________________________________________________________________________________________
# Funciones para el análisis de sensitividad

anvar1<-function( U, S, W, ecod, ewcod, wcod ) {
  Sk<-apply( S[,2:ncol(S)], 1, FUN = var )
  var<-data.frame( cod = S$cod[ ecod ] )
  
  for( j in 1:length( wcod ) ) {#j<-1
    arr<-NULL
    for( i in 1:length( ecod ) ) {#i<-1
      arr<-c( arr, var( W[ wcod[j], ] * U[ ecod[i], ewcod[j] ] ) / Sk[ ecod[i] ] )
    }
    var<-cbind( var, arr )
  }
  rm( i, j, arr )
  colnames( var )<-c( 'cod', paste( 'sen_', wcod, sep = '' ) )
  return( var )
}
