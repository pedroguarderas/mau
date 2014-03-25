#___________________________________________________________________________________________________
# Macro para crear funciones de utilidad a partir de reporte del logicaldecision
funsLogicalDecision<-defmacro( data, file, script, skip = 5, nrows = 133, expr = {

  data<-read.table( file, header = FALSE, sep = '\t', quote = NULL,
                    encoding = 'latin1', skip = skip, nrows = nrows, allowEscapes = FALSE, dec = '.',
                    fill = TRUE, strinsAsFactors = TRUE )
  data<-data[ ,!( 1:ncol(data) %in% c(3,5,8) ) ]
  data<-data.frame( data, fun = correct_char2( data, 1 ) )
  colnames(data)<-c('nom','min','max','nivel','val','a','b','c','fun')

  if ( file.exists( script ) ) {
    file.remove( script )
  }

  i<-1
  while ( i < nrow(data) ) { # i<-1
    if ( data$fun[i] != "" ) {
      f<-paste( data$fun[i], '<-function(x) { \n\tf<-NULL \n', sep = '' )
      j<-i+1
      while ( data$fun[j] == "" && j <= nrow(data) ) {
        if ( j == i + 1 ) {
          f<-paste( f, '\tif ( x >=', data$min[j], " && x <=", data$max[j], ' ) {\n',sep = '')
        } else {
          f<-paste( f, 'else if ( x >= ', data$min[j], " && x < ", data$max[j], ' ) {\n',sep = '')
        }
        if ( data$c[j] == 0.0 ) {
          f<-paste( f, '\t\tf<-(', data$b[j], ')*x + (', data$a[j], ')\n\t} ', sep = '' )
        } else {
          f<-paste( f, '\t\tf<-(', data$b[j], ')*exp( -(', data$c[j], ')*x ) + (', data$a[j], ')\n\t}', 
                    sep = '' )
        }
        j<-j+1
      }
      f<-paste( f, '\n\treturn(f)\n}', sep = '' )
      write( f, file = script, append = TRUE )
      i<-j
    }
  }
  rm(i,j,f)
})
