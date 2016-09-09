#___________________________________________________________________________________________________
# Funciones para calificación de exámenes
califica.examen<-function( examen, respuesta, vars, corresp = NULL ) {
  calf<-data.frame( examen[ , vars ] )
  
  c.resp<-1:length( respuesta )
  c.exam<-2:ncol( examen )
  
  if ( length( corresp ) > 0 ) {
    c.exam<-corresp[[1]]
    c.resp<-corresp[[2]]
  }
  calf<-data.frame( calf, 
                    t( apply( examen, 1, 
                              FUN = function(x) as.numeric( x[c.exam] == respuesta[c.resp] ) ) ) )
  rownames( calf )<-NULL
  return( calf )
}

#___________________________________________________________________________________________________
# Rasch logit
rasch.logit<-function( b, d ) {
  return( exp( b - d ) / ( 1 + exp( b - d ) ) ) 
}

#___________________________________________________________________________________________________
# Dificultad
dificultad.examen<-function( calificacion, cols ) {
  n<-ncol( calificacion )
  m<-nrow( calificacion )
  dificultad<-data.frame( colSums( calificacion[,cols], na.rm = TRUE ) )
  dificultad<-data.frame( item = names( calificacion )[cols], dificultad )
  names( dificultad )<-c( 'item', 'dificultad' )
  rownames( dificultad )<-NULL
  return( dificultad )
}

#___________________________________________________________________________________________________
# Habilidad
habilidad.examen<-function( calificacion, cols, id ) {
  n<-ncol( calificacion )
  m<-nrow( calificacion )
  habilidad<-data.frame( rowSums( calificacion[,cols], na.rm = TRUE ) )
  habilidad<-data.frame( calificacion[,id], habilidad )
  names( habilidad )<-c( names(calificacion)[id], 'habilidad' )
  rownames( habilidad )<-NULL
  return( habilidad )
}

#___________________________________________________________________________________________________
# Calificación lista de exámenes
calificacion.examenes<-function( examenes ) {
  calificacion<-list()
  for ( i in 1:length(examenes) ) {
    calf<-califica.examen( examenes[[i]]$examen, examenes[[i]]$respuesta, 
                           examenes[[i]]$vars, examenes[[i]]$corresp )
    
    dificultad<-dificultad.examen( calf, examenes[[i]]$corresp[[1]] )
    
    habilidad<-habilidad.examen(  calf, examenes[[i]]$corresp[[1]], examenes[[i]]$id ) 
    
    calificacion[[i]]<-list( carrera = examenes[[i]]$carrera,
                             forma = examenes[[i]]$forma,
                             preguntas = examenes[[i]]$preguntas,
                             vars = examenes[[i]]$vars,
                             id = examenes[[i]]$id,
                             respuestas = examenes[[i]]$corresp[[1]],
                             calificacion = calf,
                             dificultad = dificultad,
                             habilidad = habilidad )
  }
  rm( i, calf )
  
  return( calificacion )
}

#___________________________________________________________________________________________________
# Agrupación
agrupa<-function( x, grupos ) {
  return( min( which( grupos >= x ) ) )
}

#___________________________________________________________________________________________________
# Análisis de distractores
distractores.analisis<-function( examenes, calificacion, grupos ) {
  distractores<-list()
  for ( i in 1:length( examenes ) ) {
    id<-names( examenes[[i]]$examen )[examenes[[i]]$id]
    vars<-names( examenes[[i]]$examen )[examenes[[i]]$vars]
    
    distract<-merge( examenes[[i]]$examen, calificacion[[i]]$habilidad, 
                     by.x = id,
                     by.y = names( calificacion[[i]]$habilidad )[1] )
    distract<-melt( distract, id.vars = c( vars, 'habilidad' ) )
    distract$habilidad<-distract$habilidad / calificacion[[1]]$preguntas
    distract$grupo<-sapply( distract$habilidad, FUN = agrupa, grupos )
    distract$N<-1
    setnames( distract, c( 'variable', 'value' ), c( 'pregunta', 'respuesta' ) )
    distract.group<-aggregate( N ~ pregunta + grupo, distract, 
                               FUN = sum, na.action = na.pass )
    setnames( distract.group, 'N', 'NG')
    distract<-aggregate( N ~ pregunta + grupo + respuesta, distract, 
                         FUN = sum, na.action = na.pass )
    distract<-merge( distract, distract.group, by = c( 'pregunta', 'grupo' ) )
    distract<-distract[ with( distract, order( pregunta, grupo, respuesta ) ), ]
    distract$P<-distract$N / distract$NG
    rownames(distract)<-NULL
    distractores[[i]]<-list( carrera = examenes[[i]]$carrera,
                             forma = examenes[[i]]$forma,
                             preguntas = examenes[[i]]$preguntas,
                             distractor = distract )
  }
  
  return( distractores )
}

#___________________________________________________________________________________________________
# Cronbach's alpha
cronbach.alpha<-function( calificacion, cols ) {
  n<-ncol( calificacion )
  K<-n-1
  vars<-apply( calificacion[,cols], 2, FUN = var, na.rm = TRUE )
  tot.var<-var( rowSums( calificacion[,cols] ) )
  alpha<-K * ( 1 - sum( vars ) / tot.var ) / ( K - 1 )
  list( alpha = alpha, K = K, vars = vars, tot.var = tot.var )
}

#___________________________________________________________________________________________________
# Rasch's model
rasch.model<-function( calificacion, method = 'BFGS', itnmax = 1e3, lim = c( -1, 1 ), 
                       version = 1, epsilon = 10e-5 ) {
  hab<-calificacion$habilidad$habilidad
  dif<-calificacion$dificultad$dificultad
  n<-length( hab )
  m<-length( dif )
  y<-c( hab, -dif )
  I<-1:n
  J<-(n+1):(n+m)
  
  loglike<-function( x ) {
    L<-sum( x * y ) - sum( log( 1 + exp( x[I] ) %x% exp( -x[J] ) ) )
    return( L )
  }
  
  gloglike<-function( x ) {
    E<-c( exp( x[I] ), exp( -x[J] ) )
    G<-y + c( -sapply( 1:n, FUN = function(i) sum( E[i] * E[J] / ( 1 + E[i] * E[J] ) ) ), 
               sapply( (n+1):(n+m), FUN = function(j) sum( E[j] * E[I] / ( 1 + E[j] * E[I] ) ) ) )
    return( G )
  }
  
  hloglike<-function( x ) {
    E<-c( exp( x[I] ), exp( -x[J] ) )
    H<-matrix( 0, n+m, n+m )
    diag( H )<-  c( -sapply( 1:n, FUN = function(i) sum( E[i] * E[J] / ( 1 + E[i] * E[J] )^2 ) ), 
                     sapply( (n+1):(n+m), FUN = function(j) sum( E[j] * E[I] / ( 1 + E[j] * E[I] )^2 ) ) )
    H[I,J]<- E[I] %o% E[J] / ( ( 1 + E[I] %o% E[J] )^2 )
    H[J,I]<-t( H[I,J] )
    return( H )
  }
  
  Opt<-NULL
  if ( version == 1 ) {
    x0<-runif( n+m, lim[1], lim[2] )
    Opt<-optimx( par = x0, fn = loglike, gr = gloglike, 
                 method = method, hessian = FALSE, itnmax = itnmax,
                 control = list( save.failures = TRUE, trace = 0, maximize = TRUE ) )
  
  } else if ( version == 2 ) {
    x<-runif( n+m, lim[1], lim[2] )
    x0<-x
    e<-Inf
    i<-1
    while ( i <= itnmax & e > epsilon ) {
      x0<-x
      x<-x - solve( hloglike(x), gloglike(x) )
      e<-sqrt( sum( ( x - x0 ) * ( x - x0 ) ) ) / sqrt( sum( x0 * x0 ) )
      i<-i+1
    }
    Opt<-data.frame( t(x), i, loglike( x ), e )
    names( Opt )<-c( paste( 'X', 1:(n+m), sep = '' ), 'iter', 'val', 'rel.error' )
  }
  
  #   LogLike<-function( x ) {
  #     return( list( "objective" = -loglike(x), "gradient" = -gloglike(x) ) )
  #   }
  
  #   Constraint<-function( x ) {
  #     G<-cbind( c( rep( 1, n ), rep( 0, m ) ), c( rep( 0, n ), rep( 1, m ) ) )
  #     return( list( "constraints" = c( sum( x[I] ), sum( x[J] ) ), 
  #                   "jacobian" = G ) )
  #   }
  
  #   local_opts <- list( "algorithm" = "NLOPT_LD_MMA",
  #                       "xtol_rel" = 1.0e-7 )
  #   opts <- list( "algorithm" = "NLOPT_LD_AUGLAG",
  #                 "xtol_rel" = 1.0e-7,
  #                 "maxeval" = itnmax,
  #                 "local_opts" = local_opts )
  
  #   Opt<-nloptr( x0 = x0,
  #                eval_f = LogLike,
  #                eval_g_eq = Constraint,
  #                opts = opts )
  
  return( Opt )
}

#___________________________________________________________________________________________________
rasch.disc.model<-function( calificacion, par, method = 1, maxit = 1e3, 
                            epsilon = 10e-5 ) {
  h<-calificacion$habilidad$habilidad
  d<-calificacion$dificultad$dificultad
  X<-as.matrix( calificacion$calificacion[,calificacion$respuestas] )
  n<-length( h )
  m<-length( d )

  I<-1:m                     #Índice para alpha (m) (x[I])
  J<-(m+1):(2*m)             #Índice para delta (m) (x[J])
  K<-(2*m+1):(n+2*m)         #Índice para beta  (n) (x[K])
  
  loglike <- function( x ) {
    L <- x[I] %*% t( X ) %*% x[K] - sum( x[I] * x[J] * d ) - 
      sum( log( 1 + exp( x[I] %x% x[K] - rep( x[I] * x[J], times = 1, each = n ) ) ) )
    return( L )
  }
  
  gloglike<-function( x ) {
    dL <- c( sapply( 1:m, FUN = function( i ) sum( X[,i] * x[K] ) ) - d * x[J] -
             sapply( 1:m, FUN = function( i ) sum( ( x[K] - x[i+m] ) /( 1 + exp( -x[i] * ( x[K] - x[i+m] ) ) ) ) ),
             -d * x[I] + 
             sapply( 1:m, FUN = function( i ) sum( x[i] / ( 1 + exp( -x[i] * ( x[K] - x[i+m] ) ) ) ) ),
             sapply( K, FUN = function( j ) sum( X[j,] * x[I] ) ) - 
             sapply( K, FUN = function( j ) sum( x[I] / ( 1 + exp( -x[I] * ( x[j] - x[J] ) ) ) ) ) )
    
    return( dL )
  }
  
  Opt<-NULL
  # Opt<-optimx( par = x0, fn = loglike, gr = gloglike, 
  #              method = method, hessian = FALSE, itnmax = itnmax,
  #              control = list( save.failures = TRUE, trace = 0, maximize = TRUE ) )
  
  # Opt<-BBoptim( par = x0, fn = loglike, gr = gloglike, method = method, 
                # control = list( maxit = maxit, eps = epsilon, maximize = TRUE ) )
  
  Opt<-spg( par = par, fn = loglike, gr = gloglike, method = method, 
            control = list( maxit = maxit, eps = epsilon, maximize = TRUE ) )

  return( Opt )
}

#___________________________________________________________________________________________________
# Rasch análisis para una lista de examenes
rasch.analisis<-function( calificacion, method, itnmax, lim, version = 1, epsilon = 10e-5 ) {
  rasch<-list()
  for ( i in 1:length( calificacion ) ) {
    opt<-rasch.model( calificacion[[i]], method = method, itnmax = itnmax, 
                      lim = lim, version, epsilon )
    n<-nrow( calificacion[[i]]$calificacion )
    m<-n+calificacion[[i]]$preguntas
    rasch[[i]]<-list( carrera = calificacion[[i]]$carrera,
                      forma = calificacion[[i]]$forma,
                      beta = opt[1,1:n], 
                      delta = opt[1,(n+1):m],
                      info = opt[1,(m+1):ncol(opt)] )                         
  }
  rm( i, n, m)
  return( rasch )
}

#___________________________________________________________________________________________________
# Rasch análisis de estrés
rasch.analisis.estres<-function( calificacion, N, method = 'BFGS', 
                                 maxit, lim, version = 1, epsilon = 10e-5 ) {
  
  rasch<-list()
  
  for ( i in 1:length( calificacion ) ) {
    n<-nrow( calificacion[[i]]$calificacion )
    m<-n+calificacion[[i]]$preguntas
    
    nom<-names( calificacion[[i]]$calificacion )[ calificacion[[i]]$id ]
    beta.cod<-data.frame( codigo = calificacion[[i]]$calificacion[ ,calificacion[[i]]$id ] )
    delta.cod<-data.frame( pregunta = 1:calificacion[[i]]$preguntas )
    beta<-NULL
    delta<-NULL
    info<-NULL
    for ( j in 1:N ) {
      opt<-rasch.model( calificacion[[i]], 
                        method = method, itnmax = maxit, lim = lim, version, epsilon )
      beta<-rbind( beta, data.frame( beta.cod, simulacion = rep( j, n ), beta = t( opt[1,1:n] ) ) )
      delta<-rbind( delta, data.frame( delta.cod, simulacion = rep( j, m-n), delta = t( opt[1,(n+1):m] ) ) )
      info<-rbind( info, opt[1,(m+1):ncol(opt)] )
    }
    names( beta )<-c( nom, 'simulacion', 'beta' )
    names( delta )<-c( 'pregunta', 'simulacion', 'delta' )
    rownames( beta )<-NULL
    rownames( delta )<-NULL
    rasch[[i]]<-list( carrera = calificacion[[i]]$carrera,
                      forma = calificacion[[i]]$forma,
                      N = N,
                      beta = beta, 
                      delta = delta,
                      info = info )                         
  }
  rm( i, j, n, m )
  return( rasch )
}

#___________________________________________________________________________________________________
# Función que calcula la discriminación de cada ítem 
discriminacion.examen<-function( calificacion, porcentaje ){
  
  discriminacion<-list()
  for ( i in 1:length( calificacion ) ) {
    
    q<-t( apply( porcentaje[[i]], 1, FUN = function( x ) {
      q<-unlist( quantile( calificacion[[i]]$habilidad$habilidad, probs = x ) )
      return(q)
    } ) )
    q<-as.data.frame( q )
    names(q)<-c('inf','sup')
               
    id<-names( calificacion[[i]]$habilidad )[1]
    d<-merge( calificacion[[i]]$calificacion, calificacion[[i]]$habilidad, by = id )
    
    cols<-calificacion[[i]]$respuestas
    preguntas<-calificacion[[i]]$preguntas
    
    DD<-NULL
    for ( j in 1:preguntas ) {
      GS<-sum( d[ d$habilidad > q[j,'sup'], cols[j] ] )
      GI<-sum( d[ d$habilidad <= q[j,'inf'], cols[j] ] )
      D<-( GS - GI ) / max( GS, GI )
      D<-data.frame( pregunta = j, GI = GI, GS = GS, discriminacion = D )
      DD<-rbind( DD, D )
    }
    rm( GS, GI, j )
    
    discriminacion[[i]]<-list( carrera = calificacion[[i]]$carrera,
                               forma = calificacion[[i]]$forma, 
                               preguntas = calificacion[[i]]$preguntas,
                               porcentaje = porcentaje[[i]],
                               discriminacion = DD )
  }
  rm( i )
  return( discriminacion )
}
