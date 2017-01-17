#___________________________________________________________________________________________________
# Funciones para trabajar con fechas

#___________________________________________________________________________________________________
# Cálculo de meses entre dos fechas datas presentes en columnas de un data.frame
monthsbet<-function( X, cols ) {
  x<-as.Date( X[[cols[1]]] )
  y<-as.Date( X[[cols[2]]] )
  MP<-0
  
  mx<-as.numeric( format( x, "%m" ) )
  Mx<-NULL
  Fx<-NULL
  if ( mx == 12 ) {
    Mx<-as.Date( paste( as.numeric( format( x, "%Y" ) ) + 1, "-", 1, "-", 1, sep = '' ) )
  } else {
    Mx<-as.Date( paste( format( x, "%Y" ), "-", mx + 1, "-", 1, sep = '' ) )
  }
  Fx<-as.Date( paste( format( x, "%Y" ), "-", mx , "-", 1, sep = '' ) )
  Px<-( as.numeric( ( Mx - 1 - x ) ) + 1 ) / ( as.numeric( ( Mx - 1 - Fx ) ) + 1 )
  
  my<-as.numeric( format( y, '%m' ) )
  Fy<-NULL
  My<-NULL
  if ( my == 12 ) {
    My<-as.Date( paste( as.numeric( format( y, "%Y" ) ) + 1, "-", 1, "-", 1, sep = '' ) )
  } else {
    My<-as.Date( paste( format( y, "%Y" ), "-", my + 1, "-", 1, sep = '' ) )
  }
  Fy<-as.Date( paste( format( y, "%Y" ), "-", my , "-", 1, sep = '' ) )
  Py<-( as.numeric( y - Fy ) + 1 ) / ( as.numeric( ( My - 1 - Fy ) ) + 1 )
  
  v<-as.numeric( format( Fy, '%Y') ) - as.numeric( format( x, '%Y') ) 
  if ( v > 1 ) {
    MP<-MP + ( as.numeric( format( Fy, '%Y') ) - as.numeric( format( Mx, '%Y') ) - 1 ) * 12 +
      as.numeric( format( Fy, '%m') ) - 1 + 12 - as.numeric( format( x, '%m') )
  } else if ( v == 1 ) {
    MP<-as.numeric( format( Fy, '%m') ) - 1 + 12 - as.numeric( format( x, '%m') )
  } else {
    MP<-as.numeric( format( Fy, '%m') ) - as.numeric( format( Mx, '%m') )
  }
  MP<-MP + Px + Py
  return( MP )
}
