#___________________________________________________________________________________________________
library(evalpack)

m<-100
n<-130
examen<-data.frame( id = 1:m, 
                    matrix( sample( x = c('A','B','C','D'), size = m * n, replace = TRUE ), m, n ) )
respuesta<-sample( x = c('A','B','C','D'), size = n, replace = TRUE )
id.var<-'id'
calificacion<-califica.examen( examen, respuesta, 'id' )
dificultad<-dificultad.examen( calificacion )
habilidad<-habilidad.examen( calificacion )
cronbach<-cronbach.alpha( calificacion )

N<-10
Rasch<-NULL
lim<-c( -5, 5 )
for ( i in 1:N ) {
  rasch<-rasch.model( calificacion, method = 'BFGS', itnmax = 1e4, lim = lim )$solution
  Rasch<-rbind( Rasch, rasch )
}

lim<-c(-10,10)
for( i in 1:N ) {
  Z<-as.numeric( Rasch[i,1:(n+m)] )
  if ( i == 1 ) {
    plot( Z, cex = 0.5, pch = 16, ylim = lim )
  } else {
    points( Z, cex = 0.5, pch = 16 )
  }
}
MedRasch<-apply( Rasch[,1:(n+m)], 2, FUN = median, na.rm = TRUE )
MeanRasch<-apply( Rasch[,1:(n+m)], 2, FUN = mean, na.rm = TRUE )
points( MedRasch, cex = 1.5, pch = 16, col = 'red' )
points( MeanRasch, cex = 1.5, pch = 16, col = 'darkgreen' )

apply(Rasch[,1:m],1,FUN = sum)
apply(Rasch[,(m+1):(m+n)],1,FUN = sum)
