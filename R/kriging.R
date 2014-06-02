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

# conjugate_gradient<-function( A, b, x, n, e ) {
#   r<-A %*% x - b
#   d<- -r
#   alpha<- -as.numeric( ( t(r) %*% d ) / ( t(d) %*% A %*% d ) )
#   x<-x + alpha * d
#   k<-0
#   while( k < n && sqrt( t(r)%*%r ) > e ) {
#     r<- A %*% x - b
#     beta<-as.numeric( ( t(d) %*% A %*% r ) / ( t(d) %*% d ) )
#     d<- -r + beta * d
#     alpha<- -as.numeric( ( t(r) %*% d ) / ( t(d) %*% A %*% d ) )
#     x<-x + alpha * d
#     k<-k + 1
#   }
#   return( x )
# }

conjugate_gradient<-function( A, b, x, n, e ) {
  r<-b - A %*% x 
  p<-r
  g<-as.numeric( t(r) %*% r )
  k<-0
  while ( k < n & g > e ) {
    y<-A %*% p
    alpha<-g / as.numeric( t(y) %*% p )
    x<-x + alpha * p
    r<-r - alpha * y
    g0<-g
    g<-as.numeric( t(r) %*% r )
    beta<-g / g0
    p<-r + beta * p
    k<-k + 1
  }
  return( x )
}

kriging_simple_cg<-function( Z, X, x0, k, l, n, e ) {
  K<-NULL
  k0<-NULL
  L<-NULL
  d<-dim( X )
  for ( i in 1:d[2] ) { # i<-1
    K<-rbind( K, apply( X, c(2), FUN = k, X[,i] ) ) 
    k0<-rbind( k0, apply( x0, c(2), FUN = k, X[,i] ) )
  }
  for ( i in 1:dim(x0)[2] ) {
    L<-cbind( L, conjugate_gradient( K, k0[,i], l, n, e ) )
  }
  Z0<-t( L ) %*% Z
  return( list( Z0 = Z0, K = K, k0 = k0, L = L ) )
}
