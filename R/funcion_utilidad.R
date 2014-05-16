#___________________________________________________________________________________________________
# Script para construir funciones en R a partir de funciones de utilidad definidas en Logical
# Decisions

#___________________________________________________________________________________________________
# Función diseñada para leer funciones de utilidad
read_utility_functions<-function( file, script, nr, skip = 5 ) {
  options( stringsAsFactors = FALSE )
  
  funs<-read.table( file, header = FALSE, sep = '\t', quote = NULL, encoding = 'latin1', 
                    skip = skip, nrows = nr, allowEscapes = FALSE, dec = '.', fill = TRUE )
  
  funs<-funs[ ,!( 1:ncol(funs) %in% c(3,5,8) ) ]
  funs<-data.frame( funs, fun = correct_char2( funs, 1 ) )
  
  colnames(funs)<-c('nom','min','max','nivel','val','a','b','c','fun')
  
  nom<-funs$nom[1]
  nomf<-funs$fun[1]
  for ( i in 1:nrow(funs) ) {
    if ( nchar( funs$fun[i] ) == 0 )  {
      funs$nom[i]<-nom
      funs$fun[i]<-nomf
    } else {
      nom<-funs$nom[i]
      nomf<-funs$fun[i]
    }
  }
  funs<-subset( funs, complete.cases( funs ) )
  rownames( funs )<-NULL
  
  if ( file.exists( script ) ) {
    file.remove( script )
  }
  
  i<-1
  j<-1
  n<-nrow( funs )
  nomf<-funs$fun[1]
  for ( i in 1:n ) { # i<-1
    if ( funs$fun[i] != nomf || i == 1 ) {
      f<-paste( funs$fun[i], '<-function(x) { \n\tf<-0 \n', sep = '' )
      j<-i
    }
    
    if ( j == i ) {
      f<-paste( f, '\tif ( x >=', funs$min[i], " && x <=", funs$max[i], ' ) {\n',sep = '')
    } else {
      f<-paste( f, 'else if ( x >= ', funs$min[i], " && x < ", funs$max[i], ' ) {\n',sep = '')
    }
    
    if ( funs$c[i] == 0.0 ) {
      f<-paste( f, '\t\tf<-(', funs$b[i], ')*x + (', funs$a[i], ')\n\t} ', sep = '' )
    } else {
      f<-paste( f, '\t\tf<-(', funs$b[i], ')*exp( -(', funs$c[i], ')*x ) + (', funs$a[i], ')\n\t} ', 
                sep = '' )
    }
    
    nomf<-funs$fun[i]
    if ( i < n ) {
      if ( funs$fun[i+1] != nomf ) {  
        f<-paste( f, '\n\treturn(f)\n}', sep = '' )
        write( f, file = script, append = TRUE )
      }
    } else {
      f<-paste( f, '\n\treturn(f)\n}', sep = '' )
      write( f, file = script, append = TRUE )
    }
  }
  rm(i,j,f)
  return( funs )
}

#___________________________________________________________________________________________________
# Función para leer pesos del modelo
read_weights<-function( file, nrows, skip = 5, encoding = 'latin1' ) {
  data<-read.table( file, header = FALSE, sep = '\t', quote = NULL,
                    encoding = encoding, skip = skip, nrows = nrows, allowEscapes = FALSE, 
                    dec = '.', fill = TRUE )
  data<-data.frame( nom = data[,1], fnom = correct_char2( data, 1 ), 
                    weight = as.numeric( data[,2] ) / 100.0 )
  colnames( data )<-c( 'nom', 'fnom', 'weight' )
  return( data )
}

#___________________________________________________________________________________________________
# Función para evaluar indicadores con funciones de utilidad
eval_index<-function( index, names, indexsub, colcod = 1, colpos, colfun, coltip, 
                      envir = .GlobalEnv ) {
  
  data<-data.frame( cod = index[ ,colcod ] )

  for ( i in 1:nrow( names ) ) {
    # Se verifica si la función ha sido definida
    if ( names[ i, coltip ] == 'CUANTITATIVA' ) {
      if ( names[ i, colfun ] %in% ls( envir = envir ) ) {
        col<-paste( indexsub, names[ i, colpos ], sep = '' )
        data<-cbind( data, sapply( index[ , col ], FUN = names[ i, colfun ] ) )
        colnames( data )[ ncol(data) ]<-paste( 'eva_', names[ i, colpos ], sep = '' )
      }
    } else {
      col<-paste( indexsub, names[ i, colpos ], sep = '' )
      data<-cbind( data, index[ , col ] )
      colnames( data )[ ncol(data) ]<-paste( 'eva_', names[ i, colpos ], sep = '' )
    }
  }
  rm( i, col )
  return( data )
}
