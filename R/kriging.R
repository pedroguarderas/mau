#___________________________________________________________________________________________________
# Creación de Kriging
kriging_simple<-function( Z, X, x0, k ) {
  K<-NULL
  k0<-NULL
  d<-dim( X )
  for ( i in 1:d[2] ) { # i<-1
    K<-rbind( K, apply( X, c(2), FUN = k, X[,i] ) ) 
    k0<-rbind( k0, apply( x0, c(2), FUN = k, X[,i] ) )
  }
  L<-chol( K )
  J<-chol2inv( L )
  Z0<-t( k0 ) %*% J %*% Z
  return( list( Z0 = Z0, K = K, k0 = k0, L = L, J = J ) )
}

