AHPmatrix<-function( M ) {
  dim<-dim(M)
  
  for ( i in ( dim[1] - 1 ) ) {
    for ( j in (i+1):dim[2] ) {
      if ( M[i,j] != 0 ) {
        M[j,i] <- 1 / M[i,j] 
      } else {
        M[j,i] <- 0
      }
    }
  }
  rm(i,j)
  
  cols<-colSums( M )
  for ( j in 1:dim[2] ) {
    M[,j] <<- M[,j] / cols[j]
  }
}

