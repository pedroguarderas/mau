
# Standarize strings -------------------------------------------------------------------------------
#' @title Standarize strings
#' @description Función para corrección de caracteres, está diseñada para eliminar y/o reemplazar 
#' pronombres, preposiciones comunes, caracteres especiales, espacios.
#' @param data Data frame que contiene el texto a modificar.
#' @param cols Número de columna que contiene el texto a modificar.
#' @param chr Vector nulo de almacenamiento de caracteres por corregir.
#' @param rep Vector nulo de almacenamiento de caracteres de reemplazo.
#' @return Returns data table with definition of utility functions by range
#' @details Los pronombres, preposiciones comunes, caracteres especiales y espacios.
#' @author Julio Andrade, Pedro Guarderas, Andrés Lopez
#' @examples
#' x<-c( 'H?Úàn with C@1_ad1', "M¡a/¬°r&ìa} *_the#-rot",
#'       'ju%LIÖ a Pérs', '(S)tev\nén\t los cat%$' )
#' y<-sapply( x, FUN = Stand.String )
#' names( y )<-NULL
#' @import data.table
#' @import stringr
#' @export
Stand.String<-function( x, chr = NULL, rep = NULL ) {
  y<-NULL
  
  if ( length( chr ) == 0 || length( rep ) == 0 ) {
    conectors<-c( 'for', 'the', 'and', 'from', 'with', 'of', 'in',
                  'y/o', 'las', 'el ', 'para', 'o', 'en ', 'del', 'de', 'la', 'a', 'los',
                  'por', 'y' )
    conectors<-paste( '( )+', conectors, '( )+', sep = '' )
    chr<-c( '[.]', '-', '_', '[[:punct:]]', '[[:cntrl:]]',
            '[á|ä|à]', '[é|ë|è]', '[í|ï|ì]', '[ó|ö|ò]', '[ú|ü|ù]',
            conectors,
            " ( )*", '( )+$', "^( )+", ' ', '_(_)*' )
    rep<-c( ' ', ' ', ' ', '', '',
            'a', 'e', 'i', 'o', 'u', 
            rep( ' ', length( conectors ) ),
            ' ', '', '', '_', '_' )
  }
  
  if ( length( chr ) == length( rep ) ) {
    y<-tolower( x )
    y<-str_trim( y )
    for ( i in 1:length( chr ) ) {
      y<-sapply( y,
                 FUN = function( x, pattern, replacement ) str_trim( gsub( pattern, replacement, x ) ), 
                 pattern = chr[i], 
                 replacement = rep[i] )     
    }
    y<-str_trim( y )
    names( y )<-NULL
  }

  return( y )
}
