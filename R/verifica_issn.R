#___________________________________________________________________________________________________
# Verifica ISSN

verifica_issn<-function( x ) {
  check<-NULL
  issn<-unlist( str_split( x,'' ) )
  issn<-issn[ nchar( issn ) == 1 ]
  issn[ issn == 'X' ]<-'10'
  issn<-as.numeric( issn )
  
  if ( issn[8] == 10 ) {
    digit<-( sum( issn[1:7] * 8:2 ) + 10 ) %% 11
    check<-digit == 0
  } else {
    digit<-( 11 - sum( issn[1:7] * 8:2 ) %% 11 ) %% 11
    check<-digit == issn[8]
  }
  return( check )
}
