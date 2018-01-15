# Standarize strings -------------------------------------------------------------------------------
#' @title Standarize strings
#' @description Function to correct and standarize names, designed to eliminate special characters,
#' spaces and other characters.
#' @param x text to be standarized
#' @param chr character vector of replace characters
#' @param rep character vector of replacement characters
#' @return Returns data table with definition of utility functions by range
#' @author Julio Andrade, Pedro Guarderas, Andrés Lopez
#' \email{pedro.felipe.guarderas@@gmail.com}
#' @importFrom stringr str_trim
#' @examples
#'  x<-c( "H?\u00da\u00e0n with C@1_ad1", 
#'        "M\u00a1a/\u00ac\u00b0r&\u00eca *_the#-rot", 
#'        "ju%LI\u00d6 a P\u00e9rs", 
#'        "(S)tev\n\u00e9n\t los cat%$" )
#'  y<-sapply( x, FUN = Stand.String )
#'  names( y )<-NULL
#' @export
Stand.String<-function( x, chr = NULL, rep = NULL ) {
  y<-NULL
  
  if ( length( chr ) == 0 || length( rep ) == 0 ) {
    conectors<-c( 'for', 'the', 'and', 'from', 'with', 'of', 'in',
                  'y/o', 'las', 'el ', 'para', 'o', 'en ', 'del', 'de', 'la', 'a', 'los',
                  'por', 'y' )
    conectors<-paste( '( )+', conectors, '( )+', sep = '' )
    chr<-c( '[.]', '-', '_', '[[:punct:]]', '[[:cntrl:]]',
            "[\u00e1|\u00e4|\u00e0]", "[\u00e9|\u00eb|\u00e8]", "[\u00ed|\u00ef|\u00ec]", 
            "[\u00f3|\u00f6|\u00f2]", "[\u00fa|\u00fc|\u00f9]",
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
