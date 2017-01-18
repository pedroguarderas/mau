# Equity of an utility ----------------------------------------------------------------------------
func_equity<-function( f, n, D, I ) {
  x<-sort( runif( n,  D[1], D[2] ) )
  y<-sapply( x, FUN = f )
  
  equity<-NULL
  for ( i in 2:length(I) ) {
    equity<-c( equity, sum( y >= I[i-1] & y < I[i] ) / n )
  }
  
  return( equity )
}