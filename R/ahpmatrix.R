#___________________________________________________________________________________________________
# Función para normalizar matrices triangulares superiores positivas cuadradas 
# Matrices de comparación en Analytic Hierarchy Process

AHPmatrix<-function( M ) {
  d<-dim(M)
  
  for ( i in 1:( d[1] - 1 ) ) { # i<-1
    for ( j in (i+1):d[2] ) { # j<-2
      if ( M[i,j] != 0 ) {
        M[j,i] <- ( 1 / M[i,j] ) 
      } 
    }
  }
  rm(i,j)
  cols<-colSums( M )
  for ( j in 1:d[2] ) { # j<-3
    M[,j] <- M[,j] / cols[j]
  }
  return( list( row.mean = rowMeans(M), ahp = M ) )
}

