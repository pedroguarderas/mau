#___________________________________________________________________________________________________
# Macro para simular evaluaciones
sim_eval<-defmacro( N = 1, W, S, F, p, expr = {
  alpha<-p
  W<-cbind( p, t( rdirichlet( n = N-1, alpha ) ) )
  colnames( W )<-paste( 'W', 1:N, sep = '' )
  S<-as.matrix( F[ , 2:ncol(F) ] ) %*% W  
  colnames( S )<-paste( 'S', 1:ncol(S), sep = '' )
  S<-data.frame( cod = F[,1], S )
})

sim_eval_constraint<-defmacro( N = 1, W, S, F, p, pc, expr = {

  W<-matrix( 0, length(p), N - 1 )
#   rownames( W )<-paste( 'index_', 1:length(pc), sep = '' )
  
  for( i in 1:length(pc) )  {
    alpha<-100 * p[ pc[[i]][[1]] ] * pc[[i]][[2]]
    W[ pc[[i]][[1]], ]<-pc[[i]][[2]] * t( rdirichlet( n = N-1, alpha ) )
  }
  
  W<-cbind( p, W )
  colnames( W )<-paste( 'W', 1:N, sep = '' )
  S<-as.matrix( F[ , 2:ncol(F) ] ) %*% W  
  colnames( S )<-paste( 'S', 1:ncol(S), sep = '' )
  S<-data.frame( cod = F[,1], S )
})

#___________________________________________________________________________________________________
# Evalúa la equidad de una función 
func_equity<-function( f, n, D, I ) {
  x<-sort( runif( n,  D[1], D[2] ) )
  y<-sapply( x, FUN = f )
  
  equity<-NULL
  for ( i in 2:length(I) ) {
    equity<-c( equity, sum( y >= I[i-1] & y < I[i] ) / n )
  }
  
  return( equity )
}