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

