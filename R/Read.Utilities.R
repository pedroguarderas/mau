
# Builds utility functions from definition standard ------------------------------------------------
#' @title Read utilities
#' @description Builds utility functions from definition standard
#' @param file standarized file with definitions
#' @param script output script where the utility functions are defined automatically
#' @param lines number lines to read in \code{file}
#' @param skip to read the \code{file} it had to \code{skip} a given number of lines 
#' @return Returns data table with definition of utility functions by range
#' @details Details
#' @author Pedro Guarderas, Andrés Lopez
#' @seealso \code{\link{read_weights}}, \code{\link{eval_index}}
#' @examples
#' file<-'utilities.txt'
#' script<-'utilities.R'
#' lines<-17
#' skip<-2
#' encoding<-'utf-8'
#' functions<-Read.Utilities( file, script, lines, skip, encoding )
#' @export
Read.Utilities<-function( file, script, lines, skip = 2, encoding = 'utf-8' ) {

  funs<-read.table( file, header = FALSE, sep = '\t', quote = NULL, encoding = encoding, 
                    skip = skip, nrows = lines, allowEscapes = FALSE, dec = '.', fill = TRUE,
                    stringsAsFactors = FALSE )
  
  funs<-funs[ ,!( 1:ncol(funs) %in% c(3,5,8) ) ]

  funs<-data.frame( funs, fun = sapply( funs[,1], FUN = Stand.String ) )
  
  colnames( funs )<-c( 'nom','min','max','nivel','val','a','b','c','fun' )
  
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
        f<-paste( f, '\n\treturn(f)\n}\n', sep = '' )
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
