#___________________________________________________________________________________________________
# Macro para simular evaluaciones
sim_eval<-defmacro( N = 1, F, p, expr = {
# sim_eval<-defmacro( N = 1, W, S, F, p, expr = {
  alpha<-100 * p
  W<-cbind( p, t( rdirichlet( n = N, alpha ) ) )
  S<-as.matrix( F[ , 2:ncol(F) ] ) %*% W  
  colnames( S )<-paste( 'S', 1:ncol(S), sep = '' )
  S<-data.frame( cod = F[,1], S )
})

#___________________________________________________________________________________________________
# Evalúa la complejidad de una función 
comp_func<-function( f, n, a ) {
  x<-sort( runif( n ) )
  y<-sapply( x, FUN = f )
  check<-y >= a[1] & y <= a[2]
  X<-x[ check ]  
  return( list( sum( check ) / n, X ) )
}