#___________________________________________________________________________________________________
# Verifica ISSN

verifica_issn<-function( x, modify = FALSE ) {
  check<-FALSE
  issn<-unlist( str_split( x, ' ' ) )[[1]]
  
  if (  modify ) {
    l<-nchar( issn )
    if ( l < 8 ) {
      issn<-paste( paste( rep('0',8-l), collapse = '' ), issn, sep = '' )
    }
  }
    
  issn<-unlist( str_split( issn,'' ) )
  issn<-issn[ nchar( issn ) == 1 ]
  issn<-issn[ grepl( '[0-9|X]', issn ) ]
  issn[ issn == 'X' ]<-'10'
  issn<-as.numeric( issn )
  
  if ( length( issn ) == 8 ) {
    if ( issn[8] == 10 ) {
      digit<-( sum( issn[1:7] * 8:2 ) + 10 ) %% 11
      check<-digit == 0
    } else {
      digit<-( 11 - sum( issn[1:7] * 8:2 ) %% 11 ) %% 11
      check<-digit == issn[8]
    }
  } 
  return( check )
}
