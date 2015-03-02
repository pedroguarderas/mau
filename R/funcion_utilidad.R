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
  for ( i in 1:nrow(funs) ) { # i<-1
    if ( nchar( funs$fun[i] ) == 0 )  {
      funs$nom[i]<-nom
      funs$fun[i]<-nomf
    } else {
      nom<-funs$nom[i]
      nomf<-funs$fun[i]
    }
  }
  funs<-subset( funs, complete.cases( funs ) )
  funs$min<-as.numeric( funs$min )
  funs$max<-as.numeric( funs$max )
  funs$nivel<-as.numeric( funs$nivel )
  funs$val<-as.numeric( funs$val )
  funs$a<-as.numeric( funs$a )
  funs$b<-as.numeric( funs$b )
  funs$c<-as.numeric( funs$c )
  funs<-funs[ order( funs$nom, funs$min ), ]
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
      f<-paste( funs$fun[i], '<-function(x) { \n\tf<-', 0.0, '\n', sep = '' )
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
        f<-paste( f, 'else if ( x >= ', funs$max[i], ' ) {\n',sep = '')
        if ( funs$c[i] == 0.0 ) {
          f<-paste( f, '\t\tf<-(', funs$b[i], ')*', funs$max[i], 
                    ' + (', funs$a[i], ')\n\t} ', sep = '' )
        } else {
          f<-paste( f, '\t\tf<-(', funs$b[i], ')*exp( -(', funs$c[i], ')*', 
                    funs$max[i], ') + (', funs$a[i], ')\n\t} ', sep = '' )
        }
        f<-paste( f, '\n\tf<-max(0.0,f)', sep = '' )
        f<-paste( f, '\n\tf<-min(1.0,f)', sep = '' )
        f<-paste( f, '\n\treturn(f)\n}', sep = '' )
        write( f, file = script, append = TRUE )
      }
    } else {
      f<-paste( f, 'else if ( x >= ', funs$max[i], ' ) {\n',sep = '')
      if ( funs$c[i] == 0.0 ) {
        f<-paste( f, '\t\tf<-(', funs$b[i], ')*', funs$max[i], 
                  ' + (', funs$a[i], ')\n\t} ', sep = '' )
      } else {
        f<-paste( f, '\t\tf<-(', funs$b[i], ')*exp( -(', funs$c[i], ')*', 
                  funs$max[i], ') + (', funs$a[i], ')\n\t} ', sep = '' )
      }
      f<-paste( f, '\n\tf<-max(0.0,f)', sep = '' )
      f<-paste( f, '\n\tf<-min(1.0,f)', sep = '' )
      f<-paste( f, '\n\treturn(f)\n}', sep = '' )
      write( f, file = script, append = TRUE )
    }
  }
  rm(i,j,f)
  return( funs )
}

#___________________________________________________________________________________________________
# Función para leer pesos del modelo
read_weights<-function( file, cols, encoding = 'latin1' ) {
  weights<-read.xlsx( file = file, sheetIndex = 1, colIndex = cols, 
                      startRow = 1, endRow = 2 )
  weights<-t( weights[ 2:ncol( weights ) ] )
  weights<-data.frame( nom = rownames( weights ), weight = weights[,1] )
  rownames( weights )<-NULL
  weights$nom<-gsub( '\\.(\\.)*', ' ', weights$nom )
  weights$nom<-correct_char2( weights, 1 )$out
  weights$nom<-gsub( '_indicador', '', weights$nom )
  return( weights )
}


#___________________________________________________________________________________________________
# Función para evaluar indicadores con funciones de utilidad
eval_index<-function( index, names, indexsub, colcod = 1, colpos, colfun, coltip, 
                      envir = .GlobalEnv ) {
  
  data<-data.frame( cod = index[ ,colcod ] )

  for ( i in 1:nrow( names ) ) { # i<-2
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
