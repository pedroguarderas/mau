

ArrowEquity<-function( U, r ) {
  n<-ncol(U)
  m<-nrow(U)
  a<-NULL
  if ( m == length( r ) ) {
    a<-apply( U[,2:n], 2,
              FUN = function( u, r, m ) return( sum( order( u ) == order( r ) ) / m ), r, m )
  }
  rm( n, m )
  a<-as.data.frame( t( a ) )
  return( a )
}

ArrowDiscrepancy<-function( U ) {
  n<-ncol(U)
  m<-nrow(U)
  A<-data.frame()
  for ( i in 1:m ) {
    A<-rbind( A, apply( U[,2:n], 1, 
                        FUN = function( x, y, n ) return( sum( x >= y ) / n ), U[i,2:n], n-1 ) )
  }
  names( A )<-U[,1]
  rownames( A )<-U[,1]
  return( A )
}

# n<-ncol( utilidades_medicina )
# r<-S$S1
# a<-ArrowEquity( U, r )
# A<-ArrowDiscrepancy( U )

