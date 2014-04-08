#___________________________________________________________________________________________________
# Script para construir funciones en R a partir de funciones de utilidad definidas en Logical
# Decisions

# Función diseñada para leer 
read_utility_functions<-function( file, script ) {
  options( stringsAsFactors = FALSE )
  
  funs<-read.table( file, header = FALSE, sep = '\t', quote = NULL, encoding = 'latin1', 
                    skip = 5, nrows = 133, allowEscapes = FALSE, dec = '.', fill = TRUE )
  
  funs<-funs[ ,!( 1:ncol(funs) %in% c(3,5,8) ) ]
  funs<-data.frame( funs, fun = correct_char2( funs, 1 ) )
  
  colnames(funs)<-c('nom','min','max','nivel','val','a','b','c','fun')
  
  if ( file.exists( script ) ) {
    file.remove( script )
  }

  i<-1
  while ( i < nrow(funs) ) { # i<-1
    if ( funs$fun[i] != "" ) {
      f<-paste( funs$fun[i], '<-function(x) { \n\tf<-NULL \n', sep = '' )
      j<-i+1
      while ( funs$fun[j] == "" && j <= nrow(funs) ) {
        if ( j == i + 1 ) {
          f<-paste( f, '\tif ( x >=', funs$min[j], " && x <=", funs$max[j], ' ) {\n',sep = '')
        } else {
          f<-paste( f, 'else if ( x >= ', funs$min[j], " && x < ", funs$max[j], ' ) {\n',sep = '')
        }
        if ( funs$c[j] == 0.0 ) {
          f<-paste( f, '\t\tf<-(', funs$b[j], ')*x + (', funs$a[j], ')\n\t} ', sep = '' )
        } else {
          f<-paste( f, '\t\tf<-(', funs$b[j], ')*exp( -(', funs$c[j], ')*x ) + (', funs$a[j], ')\n\t}', 
                    sep = '' )
        }
        j<-j+1
      }
      f<-paste( f, '\n\treturn(f)\n}', sep = '' )
      write( f, file = script, append = TRUE )
      i<-j
    }
  }
  rm(funs,i,j,f)
}