#___________________________________________________________________________________________________
# Función para corregir caracteres

#___________________________________________________________________________________________________
owngsub<-function( x, pattern, replacement ) gsub( pattern, replacement, x )

#___________________________________________________________________________________________________
# Macro para correción de caracteres, diseñada para eliminar espacios, vocales con tíldes y colocar
# texto en minúsculas o mayúsculas
correct_char<-defmacro( data, cols, case = TRUE, expr = {
  data[,cols]<-data.frame( apply( as.data.frame( data[,cols] ), c(1,2), FUN = owngsub, 
                                  pattern = " ( )*", replacement = " " ) )
  
  if ( case ) {
    data[,cols]<-data.frame( apply( as.data.frame( data[,cols] ), c(1,2), FUN = toupper ) )
  } else {
    data[,cols]<-data.frame( apply( as.data.frame( data[,cols] ), c(1,2), FUN = tolower ) )
  }
  
  data[,cols]<-data.frame( apply( as.data.frame( data[,cols] ), c(1,2), FUN = owngsub, 
                                  pattern = "[Á|À]", replacement = "A" ) )
  data[,cols]<-data.frame( apply( as.data.frame( data[,cols] ), c(1,2), FUN = owngsub, 
                                  pattern = "[É|È]", replacement = "E" ) )
  data[,cols]<-data.frame( apply( as.data.frame( data[,cols] ), c(1,2), FUN = owngsub, 
                                  pattern = "[Í|Ì]", replacement = "I" ) )
  data[,cols]<-data.frame( apply( as.data.frame( data[,cols] ), c(1,2), FUN = owngsub, 
                                  pattern = "[Ò|Ó]", replacement = "O" ) )
  data[,cols]<-data.frame( apply( as.data.frame( data[,cols] ), c(1,2), FUN = owngsub, 
                                  pattern = "[Ù|Ú]", replacement = "U" ) )
  
  data[,cols]<-data.frame( apply( as.data.frame( data[,cols] ), c(1,2), FUN = str_trim ))
})

#___________________________________________________________________________________________________
# Correción de caracateres
correct_char2<-function( data, cols, chr = NULL, rep = NULL ) {
  out<-data[,cols]
  
  if ( length( chr ) == 0 || length( rep ) == 0 ) {
    chr<-c( ' en ', ' de ', ' la ', ' a ', ' por ', ' y ', '/', " ( )*", ' ', 'á', 'é', 'í', 'ó', 'ú', '-' )
    rep<-c( ' ', ' ', ' ', ' ', ' ', ' ', '_', ' ', '_', 'a', 'e', 'i', 'o', 'u', '_' )
  }
  
  if ( length( chr ) == length( rep ) ) {
    out<-apply( as.data.frame( out ), c(1,2), FUN = tolower )
    out<-apply( as.data.frame( out ), c(1,2), FUN = str_trim )
    for ( i in 1:length( chr ) ) {
      out<-apply( as.data.frame( out ), c(1,2), FUN = owngsub, pattern = chr[i], replacement = rep[i] )     
    }
  }
  out<-data.frame(out)
  return( out )
}
