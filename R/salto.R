#___________________________________________________________________________________________________
# Macro para el análisis de salto 

analisis_salto<-defmacro( S, expr ={
  n<-ncol(S)
  m<-nrow(S)
  D<-NULL
  T<-S[order(S[,2]),]
  for ( i in 2:n ) {
    D<-cbind( D, as.numeric( order(order(T[,i])) - order(T[,2]) ) )
  }
  D<-as.data.frame( cbind( T$cod, D ) )
  colnames( D )<-c('cod',paste('X',1:(n-1),sep=''))
  rm(n,m,T)
})
