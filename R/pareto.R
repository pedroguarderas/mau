#___________________________________________________________________________________________________
# Optimal Pareto

pareto<-function( SIM, p, m ) {
  com<-function( x, y, m, a ) {
    return( sum( x >= y ) / m >= a )
  }
  P<-matrix( 0, p, p )
  for( i in 1:p ) { # i<-2
    P[,i]<-apply( SIM, 2, FUN = com, y = SIM[,i], m = m, a = 0.9 )
  }
}

# Ejemplo

SIM<-S
SIM$cod<-NULL
p<-ncol(SIM)
m<-nrow(SIM)

pareto(SIM,p,m)

Par<-data.frame( mp = rowMeans( P ) )
Par<-data.frame( mp = rowSums( P ) )

R<-data.frame( index_name$nom, W = W[,1], WP = W[,972] )
plot( factor( index_name$nom, levels = index_name$nom ), 
      W[,1], cex=0.7, pch=16, col='red', type = 'b', ylim = c(0,0.20) )
points( factor( index_name$nom, levels = index_name$nom ),
        W[,972], cex=0.7, pch=16, col='orange', type = 'b' )

