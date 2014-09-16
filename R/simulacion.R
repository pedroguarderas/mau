#___________________________________________________________________________________________________
# Macro para simular evaluaciones
sim_eval<-defmacro( N = 1, W, S, F, p, expr = {
  alpha<-100 * p
  W<-cbind( p, t( rdirichlet( n = N, alpha ) ) )
  S<-as.matrix( F[ , 2:ncol(F) ] ) %*% W  
  colnames( S )<-paste( 'S', 1:ncol(S), sep = '' )
  S<-data.frame( cod = F[,1], S )
})

#___________________________________________________________________________________________________
# Evalúa la complejidad de una función 
func_equity<-function( f, n, D, I ) {
  x<-sort( runif( n,  D[1], D[2] ) )
  y<-sapply( x, FUN = f )
  
  equity<-NULL
  for ( i in 2:length(I) ) {
    equity<-c( equity, sum( y >= I[i-1] & y < I[i] ) / n )
  }
  
  return( equity )
}