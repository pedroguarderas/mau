#___________________________________________________________________________________________________
library(evalpack)
library(nloptr)

n<-1000
m<-130
examen<-data.frame( id = 1:n, 
                    matrix( sample( x = c('A','B','C','D'), size = m * n, replace = TRUE ), n, m ) )
respuesta<-sample( x = c('A','B','C','D'), size = m, replace = TRUE )
id.var<-'id'
calificacion<-califica.examen( examen, respuesta, 'id' )
dificultad<-dificultad.examen( calificacion )
habilidad<-habilidad.examen( calificacion )
cronbach<-cronbach.alpha( calificacion )

N<-30
Rasch<-list()
lim<-c( -1, 1 )
for ( i in 1:N ) {
  cat( paste( '\r', i, sep = '' ) )
  rasch<-rasch.model( calificacion, method = 'BFGS', itnmax = 1e3, lim = lim )
  Rasch[[i]]<-rasch
}

lim<-c(-60,60)
for( i in 1:N ) {
  Z<-as.numeric( Rasch[[i]]$solution )
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

apply(Rasch[,1:n],1,FUN = sum)
apply(Rasch[,(n+1):(n+m)],1,FUN = sum)

sum( x[1:n] )
sum( x[(n+1):(n+m)] )

lapply( Rasch, FUN = function(x) x$iteration )
rasch$iterations

as.numeric( Opt[1,1:(n+m)] ) - x
plot( sort( x ), pch = 16, cex = 0.5 )
points( sort( as.numeric( Opt[1,1:(n+m)] ) ), pch = 16, cex = 0.5, col = 'red3' )
