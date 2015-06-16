#___________________________________________________________________________________________________
# Función para corregir caracteres

#___________________________________________________________________________________________________
owngsub<-function( x, pattern, replacement ) str_trim( gsub( pattern, replacement, x ) )

#___________________________________________________________________________________________________
# Macro para corrección de caracteres, diseñada para eliminar espacios, vocales con tíldes y colocar
# texto en minúsculas o mayúsculas
correct_char<-defmacro( data, cols, case = TRUE, expr = {
  data[,cols]<-data.frame( apply( as.data.frame( data[,cols] ), c(1,2), FUN = owngsub, 
                                  pattern = " ( )*", replacement = " " ) )
  
  if ( case ) {
    data[,cols]<-data.frame( apply( as.data.frame( data[,cols] ), c(1,2), FUN = toupper ) )
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
  } else {
    data[,cols]<-data.frame( apply( as.data.frame( data[,cols] ), c(1,2), FUN = tolower ) )
    data[,cols]<-data.frame( apply( as.data.frame( data[,cols] ), c(1,2), FUN = owngsub, 
                                    pattern = "[á|à]", replacement = "a" ) )
    data[,cols]<-data.frame( apply( as.data.frame( data[,cols] ), c(1,2), FUN = owngsub, 
                                    pattern = "[é|è]", replacement = "e" ) )
    data[,cols]<-data.frame( apply( as.data.frame( data[,cols] ), c(1,2), FUN = owngsub, 
                                    pattern = "[í|ì]", replacement = "i" ) )
    data[,cols]<-data.frame( apply( as.data.frame( data[,cols] ), c(1,2), FUN = owngsub, 
                                    pattern = "[ó|ò]", replacement = "o" ) )
    data[,cols]<-data.frame( apply( as.data.frame( data[,cols] ), c(1,2), FUN = owngsub, 
                                    pattern = "[ú|ù]", replacement = "u" ) )
  }
  
  data[,cols]<-data.frame( apply( as.data.frame( data[,cols] ), c(1,2), FUN = str_trim ))
})

#___________________________________________________________________________________________________
# Corrección de caracteres 2
correct_char2<-function( data, cols, chr = NULL, rep = NULL ) {
  out<-data[,cols]
  
  if ( length( chr ) == 0 || length( rep ) == 0 ) {
    chr<-c( ' y/o ', ' las ', ' el ', ' para ', ' o ', ' en ', ' del ', ' de ', ' la ', ' a ', 
            ' por ', ' y ', '/', " ( )*", ' ', 'á', 'é', 'í', 'ó', 'ú', '-', '\\(', '\\)', '( )$', 
            '_(_)*' )
    rep<-c( ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', '_', ' ', '_', 
            'a', 'e', 'i', 'o', 'u', '_', '', '', '', '_' )
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
