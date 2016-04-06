#___________________________________________________________________________________________________
# Funciones para calificación de exámenes
califica.examen<-function( examen, respuesta, id.var, corresp = NULL ) {
  calf<-data.frame( examen[ , id.var ] )
  names( calf )<-id.var
  
  c.resp<-1:length( respuesta )
  c.exam<-2:ncol( examen )
  
  if ( !is.null( corresp ) ) {
    c.exam<-corresp[,1]
    c.resp<-corresp[,2]
  }
  calf<-data.frame( id.var = calf, 
                    t( apply( examen, 1, FUN = function(x) as.numeric( x[c.exam] == respuesta[c.resp] ) ) ) )
  rownames( calf )<-NULL
  return( calf )
}

#___________________________________________________________________________________________________
# Dificultad
dificultad.examen<-function( calificacion ) {
  n<-ncol( calificacion )
  m<-nrow( calificacion )
  dificultad<-data.frame( colSums( calificacion[,2:n] ) )
  dificultad<-data.frame( item = names( calificacion )[2:n], dificultad )
  names( dificultad )<-c( 'item', 'dificultad' )
  rownames( dificultad )<-NULL
  return( dificultad )
}

#___________________________________________________________________________________________________
# Habilidad
habilidad.examen<-function( calificacion ) {
  n<-ncol( calificacion )
  m<-nrow( calificacion )
  habilidad<-data.frame( rowSums( calificacion[,2:n] ) )
  habilidad<-data.frame( calificacion[,1], habilidad )
  names( habilidad )<-c( names(calificacion)[1], 'habilidad' )
  rownames( habilidad )<-NULL
  return( habilidad )
}

#___________________________________________________________________________________________________
# Cronbach's alpha
cronbach.alpha<-function( calificacion ) {
  n<-ncol( calificacion )
  K<-n-1
  vars<-apply( calificacion[,2:n], 2, FUN = var )
  tot.var<-var( rowSums( calificacion[,2:n] ) )
  alpha<-K * ( 1 - sum( vars ) / tot.var ) / ( K - 1 )
  list( alpha = alpha, K = K, vars = vars, tot.var = tot.var )
}

#___________________________________________________________________________________________________
# Rasch's model
rasch.model<-function( calificacion, method = 'BFGS', itnmax = 1e3, lim = c( -8, 8 ) ) {
  hab<-habilidad.examen( calificacion )$habilidad
  dif<-dificultad.examen( calificacion )$dificultad
  n<-length( hab )
  m<-length( dif )
  y<-c( hab, dif )
  I<-1:n
  J<-(n+1):(n+m)
  
  loglike<-function( x ) {
    L<-sum( x * y ) - sum( log( 1 + exp( x[I] ) %x% exp( x[J] ) ) )
    return( L )
  }
  
  gloglike<-function( x ) {
    E<-c( exp( x[I] ), exp( x[J] ) )
    G<-y - c( sapply( 1:n, FUN = function(i) sum( E[i] * E[J] / ( 1 + E[i] * E[J] ) ) ), 
              sapply( (n+1):(n+m), FUN = function(j) sum( E[j] * E[I] / ( 1 + E[j] * E[I] ) ) ) )
    return( G )
  }
  
  hloglike<-function( x ) {
    E<-c( exp( x[I] ), exp( x[J] ) )
    sb<-sum( E[I] )
    sd<-sum( E[J] )
    H<-matrix( 0, n+m, n+m )
    diag( H )<- -c( sapply( 1:n, FUN = function(i) sum( E[i] * E[J] / ( 1 + E[i] * E[J] )^2 ) ), 
                    sapply( (n+1):(n+m), FUN = function(j) sum( E[j] * E[I] / ( 1 + E[j] * E[I] )^2 ) ) )
    H[I,J]<- -E[I] %o% E[J] / ( ( 1 + E[I] %o% E[J] )^2 )
    H[J,I]<-t( H[I,J] )
    return( H )
  }
  
  x0<-runif( n+m, lim[1], lim[2] )
  Opt<-optimx( par = x0, fn = loglike, gr = gloglike, 
               method = method, hessian = FALSE, itnmax = itnmax,
               control = list( save.failures = TRUE, trace = 0, maximize = TRUE ) )
  
#   LogLike<-function( x ) {
#     return( list( "objective" = -loglike(x), "gradient" = -gloglike(x) ) )
#   }
#   Constraint<-function( x ) {
#     G<-cbind( c( rep( 1, n ), rep( 0, m ) ), c( rep( 0, n ), rep( 1, m ) ) )
#     return( list( "constraints" = c( sum( x[I] ), sum( x[J] ) ), 
#                   "jacobian" = G ) )
#   }
#   
#   local_opts <- list( "algorithm" = "NLOPT_LD_MMA",
#                       "xtol_rel" = 1.0e-7 )
#   opts <- list( "algorithm" = "NLOPT_LD_AUGLAG",
#                 "xtol_rel" = 1.0e-7,
#                 "maxeval" = itnmax,
#                 "local_opts" = local_opts )
#   
#   Opt<-nloptr( x0 = x0,
#                eval_f = LogLike,
#                eval_g_eq = Constraint,
#                opts = opts )

#   x<-runif( n+m, lim[1], lim[2] )
#   for ( i in 1:100 ) {
#     H<-hloglike(x)
#     x0<-x
#     x<-x - solve( H + 1e-10, gloglike(x) )
#     print( paste( 'it: ', i, 
#                 ', err.rel: ', 
#                 formatC( sqrt( sum( (x-x0)*(x-x0) ) ), format = 'f', digits = 20 ),
#                 ', val: ', formatC( loglike(x), format = 'f', digits = 20 ), 
#                 sep = '' ) )
#   }
  
  return( Opt )
}
