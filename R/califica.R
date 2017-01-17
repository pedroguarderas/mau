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

rasch.info<-function( b, d ) {
  return( exp( b - d ) / ( ( 1 + exp( b - d ) )^2 ) ) 
}

#___________________________________________________________________________________________________
# Rasch logit discriminante
rasch.logit.disc<-function( b, a, d ) {
  return( exp( a * ( b - d ) ) / ( 1 + exp( a * ( b - d ) ) ) ) 
}

rasch.disc.info<-function( b, a, d ) {
  return( ( a^2 ) * exp( a * ( b - d ) ) / ( ( 1 + exp( a * ( b - d ) ) )^2 ) ) 
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
                             banco = examenes[[i]]$banco,
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
                             banco = examenes[[i]]$banco,
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
  tot.var<-var( rowSums( calificacion[,cols], na.rm = TRUE ) )
  alpha<-K * ( 1 - sum( vars ) / tot.var ) / ( K - 1 )
  list( alpha = alpha, K = K, vars = vars, tot.var = tot.var )
}

#___________________________________________________________________________________________________
# Rasch's model
rasch.loglike<-function( beta, delta, habilidad, dificultad ) {
  L<-sum( beta * habilidad ) - sum( delta * dificultad ) - sum( log( 1 + exp( beta ) %x% exp( -delta ) ) )
  return( L )
}

rasch.gloglike<-function( beta, delta, habilidad, dificultad ) {
  m<-length( beta )
  n<-length( delta )
  Eb<-exp( beta )
  Ed<-exp( -delta )
  G<-c( -dificultad, habilidad ) + 
    c( sapply( 1:n, FUN = function(j) sum( Ed[j] * Eb / ( 1 + Ed[j] * Eb ) ) ),
       -sapply( 1:m, FUN = function(i) sum( Eb[i] * Ed / ( 1 + Eb[i] * Ed ) ) ) )
  return( G )
}

rasch.gloglike.beta.fix<-function( delta, beta, habilidad, dificultad ) {
  n<-length( delta )
  Eb<-exp( beta )
  Ed<-exp( -delta )
  G<-( -dificultad ) + sapply( 1:n, FUN = function(j) sum( Ed[j] * Eb / ( 1 + Ed[j] * Eb ) ) )
  return( G )
}

# rasch.hloglike<-function( beta, delta ) {
#   E<-c( exp( beta ), exp( -delta ) )
#   H<-matrix( 0, n+m, n+m )
#   diag( H )<-  c( -sapply( 1:n, FUN = function(i) sum( E[i] * E[J] / ( 1 + E[i] * E[J] )^2 ) ), 
#                   sapply( (n+1):(n+m), FUN = function(j) sum( E[j] * E[I] / ( 1 + E[j] * E[I] )^2 ) ) )
#   H[I,J]<- E[I] %o% E[J] / ( ( 1 + E[I] %o% E[J] )^2 )
#   H[J,I]<-t( H[I,J] )
#   return( H )
# }

rasch.model<-function( calificacion, beta.fix = TRUE, 
                       method = 'BFGS', itnmax = 1e3, lim = c( -1, 1 ), version = 1, epsilon = 10e-5 ) {
  h<-calificacion$habilidad$habilidad
  d<-calificacion$dificultad$dificultad
  m<-length( h )
  n<-length( d )
  
  I<-1:n
  J<-(n+1):(m+n)
  
  beta<-NULL
  loglike<-NULL
  gloglike<-NULL
  x0<-NULL
  if ( beta.fix ) {
    beta<-( h - mean( h ) ) / sd( h )
    loglike<-function( x ) return( rasch.loglike( beta, x, h, d ) )
    gloglike<-function( x ) return( rasch.gloglike.beta.fix( x, beta, h, d ) )
    x0<-runif( n, lim[1], lim[2] )
  } else {
    loglike<-function( x ) return( rasch.loglike( x[J], x[I], h, d ) )
    gloglike<-function( x ) return( rasch.gloglike( x[J], x[I], h, d ) )
    x0<-runif( m + n, lim[1], lim[2] )
  }

  Opt<-NULL
  Opt<-optimx( par = x0, fn = loglike, gr = gloglike, 
               method = method, hessian = FALSE, itnmax = itnmax,
               control = list( save.failures = TRUE, trace = 0, maximize = TRUE ) )
  
  if ( beta.fix ) {
    Opt<-list( delta = as.numeric( Opt[1,1:n] ),
               beta = beta,
               info = Opt[1,(n+1):ncol(Opt) ] )
  } else {
    Opt<-list( delta = as.numeric( Opt[1,1:n] ),
               beta = as.numeric( Opt[1,(n+1):(n+m)] ),
               info = Opt[1,(n+m+1):ncol(Opt) ] )
  }
  return( Opt )
}

#___________________________________________________________________________________________________
# Rasch análisis para una lista de examenes
rasch.analisis<-function( calificacion, beta.fix = TRUE, method = 'BFGS', itnmax = 500, lim, version = 1, epsilon = 10e-4 ) {
  rasch<-list()
  for ( i in 1:length( calificacion ) ) {
    opt<-rasch.model( calificacion[[i]], beta.fix = beta.fix, method = method, itnmax = itnmax, 
                      lim = lim, version, epsilon )
    n<-nrow( calificacion[[i]]$calificacion )
    m<-n+calificacion[[i]]$preguntas
    
    rasch[[i]]<-list( carrera = calificacion[[i]]$carrera,
                      forma = calificacion[[i]]$forma,
                      banco = calificacion[[i]]$banco,
                      beta = opt$beta, 
                      delta = opt$delta,
                      info = opt$info )                         
  }
  rm( i, n, m)
  return( rasch )
}

#___________________________________________________________________________________________________
# Rasch discriminant model
rasch.disc.loglike<-function( alpha, delta, beta, dificultad, X ) {
  n<-length( beta )
  L <- as.numeric( alpha %*% t( X ) %*% beta ) - sum( alpha * delta * dificultad ) - 
    sum( log( 1 + exp( alpha %x% beta - rep( alpha * delta, times = 1, each = n ) ) ) )
  return( L )
}

rasch.disc.gloglike<-function( alpha, delta, beta, dificultad, X ) {
  n<-length( alpha )
  m<-length( beta )
  G<-c( sapply( 1:n, FUN = function( i ) sum( X[,i] * beta ) ) - dificultad * delta -
        sapply( 1:n, FUN = function( i ) sum( ( beta - delta[i] ) /( 1 + exp( -alpha[i] * ( beta - delta[i] ) ) ) ) ),
        -dificultad * alpha + 
        sapply( 1:n, FUN = function( i ) sum( alpha[i] / ( 1 + exp( -alpha[i] * ( beta - delta[i] ) ) ) ) ),
        sapply( 1:m, FUN = function( j ) sum( X[j,] * alpha ) ) - 
        sapply( 1:m, FUN = function( j ) sum( alpha / ( 1 + exp( -alpha * ( beta[j] - delta ) ) ) ) ) )
  
  return( G )
}

rasch.disc.gloglike.beta.fix<-function( alpha, delta, beta, dificultad, X ) {
  n<-length( alpha )
  m<-length( beta )
  G<-c( sapply( 1:n, FUN = function( i ) sum( X[,i] * beta ) ) - dificultad * delta -
        sapply( 1:n, FUN = function( i ) sum( ( beta - delta[i] ) /( 1 + exp( -alpha[i] * ( beta - delta[i] ) ) ) ) ),
        -dificultad * alpha + 
        sapply( 1:n, FUN = function( i ) sum( alpha[i] / ( 1 + exp( -alpha[i] * ( beta - delta[i] ) ) ) ) ) )
  
  return( G )
}

rasch.disc.model<-function( calificacion, beta.fix = TRUE, method = 'BFGS', maxit = 1e3, epsilon = 10e-5 ) {
  h<-calificacion$habilidad$habilidad
  d<-calificacion$dificultad$dificultad
  X<-as.matrix( calificacion$calificacion[,calificacion$respuestas] )
  X<-apply( X, c(1,2), FUN = function( x ) ifelse( is.na(x), 0, x ) )
  m<-length( h )
  n<-length( d )

  I<-1:n                     #Índice para alpha (m) (x[I])
  J<-(n+1):(2*n)             #Índice para delta (m) (x[J])
  K<-(2*n+1):(m+2*n)         #Índice para beta  (n) (x[K])
  
  beta<-NULL
  loglike<-NULL
  gloglike<-NULL
  x0<-NULL
  if ( beta.fix ) {
    beta<-( h - mean( h ) ) / sd( h )
    loglike<-function( x ) return( rasch.disc.loglike( x[I], x[J], beta, d, X ) )
    gloglike<-function( x ) return( rasch.disc.gloglike.beta.fix( x[I], x[J], beta, d, X ) )
    x0<-runif( 2 * n, -1, 1 )
  } else {
    loglike<-function( x ) return( rasch.disc.loglike( x[I], x[J], x[K], d, X ) )
    gloglike<-function( x ) return( rasch.disc.gloglike( x[I], x[J], x[K], d, X ) )
    x0<-runif( m + 2 * n, -1, 1 )
  }
  
  Opt<-NULL
  Opt<-optimx( par = x0, fn = loglike, gr = gloglike,
               method = method, hessian = FALSE, itnmax = maxit,
               control = list( save.failures = TRUE, trace = 0, maximize = TRUE ) )
  
  if ( beta.fix ) {
    Opt<-list( alpha = as.numeric( Opt[1,1:n] ), 
               delta = as.numeric( Opt[1,(n+1):(2*n)] ),
               beta = beta,
               info = Opt[1,(2*n+1):ncol(Opt) ] )
  } else {
    Opt<-list( alpha = as.numeric( Opt[1,1:n] ), 
               delta = as.numeric( Opt[1,(n+1):(2*n)] ),
               beta = as.numeric( Opt[1,(2*n+1):(m+2*n)] ),
               info = Opt[1,(m+2*n+1):ncol(Opt) ] )
  }

  return( Opt )
}

#___________________________________________________________________________________________________
# Discriminant rasch analysis
rasch.disc.analisis<-function( calificacion, beta.fix = TRUE, method = 'BFGS', maxit = 500, epsilon = 10e-4 ) {
  rasch.disc<-list()
  for ( i in 1:length( calificacion ) ) {
    m<-length( calificacion[[i]]$habilidad$habilidad )
    n<-length( calificacion[[i]]$dificultad$dificultad )

    opt<-rasch.disc.model( calificacion = calificacion[[i]], beta.fix = beta.fix,
                           method = method, maxit = maxit, epsilon = epsilon )
    
    rasch.disc[[i]]<-list( carrera = calificacion[[i]]$carrera,
                           forma = calificacion[[i]]$forma,
                           banco = calificacion[[i]]$banco,
                           alpha = opt$alpha, 
                           delta = opt$delta,
                           beta = opt$beta,
                           info = opt$info )   

  }
  return( rasch.disc )
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
                      banco = calificacion[[i]]$banco,
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
    
    J<-calificacion[[i]]$respuestas
    M<-calificacion[[i]]$preguntas
    
    DD<-NULL
    for ( j in 1:M ) {
      p<-as.numeric( porcentaje[[i]][j,] )
      H<-calificacion[[i]]$habilidad$habilidad
      q<-unlist( quantile( H, probs = p ) )
      IS<-( H >= q[2] )
      II<-( H <= q[1] )
      NS<-sum( IS, na.rm = TRUE )
      NI<-sum( II, na.rm = TRUE )
      GS<-sum( calificacion[[i]]$calificacion[ IS,J[j] ], na.rm = TRUE )
      GI<-sum( calificacion[[i]]$calificacion[ II,J[j] ], na.rm = TRUE )
      ID<-GS / NS - GI / NI
      D<-data.frame( reactivo = j, NI = NI, GI = GI, NS = NS, GS = GS, ID = ID )
      DD<-rbind( DD, D )
    }
    
    discriminacion[[i]]<-list( carrera = calificacion[[i]]$carrera,
                               forma = calificacion[[i]]$forma, 
                               preguntas = calificacion[[i]]$preguntas,
                               banco = calificacion[[i]]$banco,
                               discriminacion = DD )
  }
  rm( i )
  return( discriminacion )
}

#___________________________________________________________________________________________________
# Correlación de punto biserial
corr.punto.biserial<-function( calificacion ) {
  punto.biserial<-list()
  for ( i in 1:length( calificacion ) ) {
    P<-calificacion[[i]]$preguntas
    N<-nrow( calificacion[[i]]$calificacion )
    pbis<-NULL
    for ( k in 1:P ) {
      ux<-mean( calificacion[[i]]$habilidad$habilidad / P )
      j<-calificacion[[i]]$respuestas[k]
      K<-which( calificacion[[i]]$calificacion[,j] == 1 )
      u<-mean( calificacion[[i]]$habilidad$habilidad[ K ] / P )
      s<-sd( calificacion[[i]]$habilidad$habilidad / P )
      p<-calificacion[[i]]$dificultad$dificultad[k] / N
      q<-1 - p
      pbis<-c( pbis, ( u - ux ) * sqrt( p / q ) / s )
    }
    punto.biserial[[i]]<-list( carrera = calificacion[[i]]$carrera,
                               forma = calificacion[[i]]$forma,
                               banco = calificacion[[i]]$banco,
                               corr.pbis = data.table( reactivo = 1:P, pbis = pbis ) )
  }
  return( punto.biserial )
}

#___________________________________________________________________________________________________
rasch.infit.outfit<-function( calificacion, rasch.disc ) {
  rasch.in.out<-list()
  for ( i in 1:length( calificacion ) ) {
    
    N<-nrow( calificacion[[i]]$calificacion )
    M<-calificacion[[i]]$preguntas
    J<-calificacion[[i]]$respuestas
    a<-rasch.disc[[i]]$alpha
    b<-rasch.disc[[i]]$beta
    d<-rasch.disc[[i]]$delta
    
    outfit<-NULL
    infit<-NULL
    sdoutfit<-NULL
    sdinfit<-NULL
    tout<-NULL
    tinf<-NULL
    
    for ( j in 1:M ) {
      p<-sapply( b, rasch.logit.disc, a = a, d = d )
      x<-calificacion[[i]]$calificacion[,J[j]]
      x[ is.na( x ) ]<-0
      
      w<-p * ( 1 - p )
      U<-( x - p )^2
      
      out<-sum( U / w, na.rm = TRUE ) / N
      inf<-sum( U, na.rm = TRUE ) / sum( w, na.rm = TRUE )
      
      sdout<-sqrt( sum( 1 / w, na.rm = TRUE ) - 4 * N ) / N
      sdinf<-sqrt( sum( w, na.rm = TRUE ) - 4 * sum( w^2, na.rm = TRUE ) ) / sum( w, na.rm = TRUE )
      
      outfit<-c( outfit, out )
      infit<-c( infit, inf )
      
      sdoutfit<-c( sdoutfit, sdout )
      sdinfit<-c( sdinfit, sdinf )
      
      tout<-c( tout, ( out^(1/3) - 1 ) * ( 3 / sdout ) + sdout / 3 )
      tinf<-c( tinf, ( inf^(1/3) - 1 ) * ( 3 / sdinf ) + sdinf / 3 )
    }
    rasch.in.out[[i]]<-data.frame( reactivo = 1:M, banco = calificacion[[i]]$banco,
                                   outfit = outfit, sdoutfit = sdoutfit, tout = tout,
                                   infit = infit, sdinfit = sdinfit, tinf = tinf )
  }
  return( rasch.in.out )
}

#___________________________________________________________________________________________________
rasch.ECIXz<-function( calificacion, rasch.disc ) {
  rasch.ECI<-list()
  for ( i in 1:length( calificacion ) ) {
    N<-nrow( calificacion[[i]]$calificacion )
    M<-calificacion[[i]]$preguntas
    J<-calificacion[[i]]$respuestas
    a<-rasch.disc[[i]]$alpha
    b<-rasch.disc[[i]]$beta
    d<-rasch.disc[[i]]$delta
    
    ECI2z<-NULL
    ECI4z<-NULL
    
    e2<-NULL
    e4<-NULL
    
    for ( j in 1:M ) {
      p<-sapply( b[1,], rasch.logit.disc, a = a[1,j], d = d[1,j] )
      x<-calificacion[[i]]$calificacion[,J[j]]
      x[ is.na( x ) ]<-0
      
      G<-( 1/M )*sum( p )
      uG<-( 1/N )*sum( G )
      uP<-( 1/N )*sum( p )
      Q<-1-p
      
      ECI2z[j]<-sum( ( p - x ) * ( G - uG ) ) / ( sum( p*Q*( G-uG )^2 ) )^(1/2)
      ECI4z[j]<-sum( ( p - x ) * ( p - uP ) ) / ( sum( p*Q*( G-uP )^2 ) )^(1/2)
    }
    rasch.ECI[[i]]<-data.frame( reactivo = 1:M, ECI2z = ECI2z, ECI4z = ECI4z )
  }
  return( rasch.ECI )
}