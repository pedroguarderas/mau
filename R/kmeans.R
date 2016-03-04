distancia<-function( X, M ) {
  n<-ncol( X )
  m<-nrow( X )
  
  D<-X %*% M %*% t( X )
  for ( i in 1:(m-1) ) {
    for ( j in (i+1):m ) {
      D[i,j]<-D[i,i] + D[j,j] - 2 * D[i,j]
      if ( i != j ) {
        D[j,i]<-D[i,j]
      }
    }
  }
  diag(D)<-0
  return( D )
}

distancia.center<-function( X, C, M ) {
  n<-nrow( C )
  m<-nrow( X )
  
  D<-matrix( 0, m, n )
  for ( i in 1:m ) {
    for ( j in 1:n ) {
      D[i,j]<-( X[i,] - C[j,] ) %*% ( M %*% ( X[i,] - C[j,] ) )
    }
  }
  diag(D)<-0
  return( D )
}

choose.min<-function( x ) {
  return( sample( which.min( x ), size = 1 ) )
}

Kmeans<-function( X, M, I = 10, N = 5 ) {
  K<-data.frame( X )
  n<-nrow( K )
  m<-ncol( K )
  C<-sample(1:n,N)
  C<-as.matrix( K[C,] )
  
  for ( i in 1:I ) {
    D<-distancia.center( X, C, M )
    K$cluster<-apply( D, 1, FUN = choose.min )
    C<-melt( K, id.vars = 'cluster' )
    C<-aggregate( value ~ cluster + variable, C, mean )
    C<-dcast( data = C, cluster ~ variable, value.var = 'value')
    C$cluster<-NULL
    C<-as.matrix( C )
  }
  return( list( cluster = K, centroid = C, distnace = D ) )
}