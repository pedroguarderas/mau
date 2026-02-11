# Builds utility functions from definition standard ------------------------------------------------
#' @title Read utilities
#' @description Builds utility functions from definition standard.
#' @param file standardize file with definitions.
#' @param lines number lines to read in \code{file}.
#' @param skip to read the \code{file} it had to \code{skip} a given number of lines.
#' @param encoding file encoding.
#' @param envir environment where utility functions will be created.
#' @return Returns data table with definition of utility functions by range.
#' @details The basic MAUT models are built with functions of constant absolute risk aversion, 
#' this functions could be defined with simple parameters, only is necessary a function name and
#' the domain of definition of every function and more important is necessary no more than three
#' coefficients for the function definition.
#' @author Pedro Guarderas, Andrés Lopez
#' @seealso \code{\link{stand_string}}
#' @examples
#' library( data.table )
#' file <- system.file("extdata", "utilities.txt", package = "mau" )
#' lines <- 17
#' skip <- 2
#' encoding <- 'utf-8'
#' functions <- read_utilities( file, lines, skip, encoding )
#' @importFrom utils read.csv read.table
#' @export
read_utilities <- function( file, lines, skip = 2, encoding = 'utf-8', envir = .GlobalEnv ) {
  
  funs <- read.table( file, header = FALSE, sep = '\t', quote = NULL, encoding = encoding, 
                      skip = skip, nrows = lines, allowEscapes = FALSE, dec = '.', fill = TRUE,
                      colClasses = c( 'character', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric' ),
                      stringsAsFactors = FALSE )
  
  funs <- data.frame( funs, fun = sapply( funs[,1], FUN = stand_string ), stringsAsFactors = FALSE )
  
  colnames( funs ) <- c( 'nom', 'min', 'max', 'a', 'b', 'c', 'fun' )
  
  nom <- funs$nom[1]
  nomf <- funs$fun[1]
  for ( i in 1:nrow(funs) ) { # i <- 1
    if ( nchar( funs$fun[i] ) == 0 )  {
      funs$nom[i] <- nom
      funs$fun[i] <- nomf
    } else {
      nom <- funs$nom[i]
      nomf <- funs$fun[i]
    }
  }
  funs <- subset( funs, complete.cases( funs ) )
  funs <- funs[ order( funs$nom, funs$min ), ]
  rownames( funs ) <- NULL

  
  i <- 1
  j <- 1
  n <- nrow( funs )
  nomf <- funs$fun[1]
  strf <- NULL
  for ( i in 1:n ) { # i <- 1
    
    if ( funs$fun[i] != nomf || i == 1 ) {
      f <- paste0( funs$fun[i], ' <- function(x) { \n\tf <- ', 0.0, '\n' )
      j <- i
    }
    
    if ( j == i ) {
      f <- paste0( f, '\tif ( x >=', funs$min[i], " && x <=", funs$max[i], ' ) {\n' )
    } else {
      f <- paste0( f, 'else if ( x >= ', funs$min[i], " && x < ", funs$max[i], ' ) {\n' )
    }
    
    if ( funs$c[i] == 0.0 ) {
      f <- paste0( f, '\t\tf <- (', funs$b[i], ')*x + (', funs$a[i], ')\n\t} ' )
    } else {
      f <- paste0( f, '\t\tf <- (', funs$b[i], ')*exp( -(', funs$c[i], ')*x ) + (', funs$a[i], ')\n\t} ', 
                  sep = '' )
    }
    
    nomf <- funs$fun[i]
    if ( i < n ) {
      if ( funs$fun[i+1] != nomf ) {
        f <- paste0( f, 'else if ( x >= ', funs$max[i], ' ) {\n' )
        if ( funs$c[i] == 0.0 ) {
          f <- paste0( f, '\t\tf <- (', funs$b[i], ')*', funs$max[i], 
                      ' + (', funs$a[i], ')\n\t} ' )
        } else {
          f <- paste0( f, '\t\tf <- (', funs$b[i], ')*exp( -(', funs$c[i], ')*', 
                      funs$max[i], ') + (', funs$a[i], ')\n\t} ' )
        }
        f <- paste0( f, '\n\tf <- max(0.0,f)' )
        f <- paste0( f, '\n\tf <- min(1.0,f)' )
        f <- paste0( f, '\n\treturn(f)\n}\n' )
        
        strf <- paste0( strf, '\n', f )
        
        eval( parse( text = f ), envir = envir )
        
      }
    } else {
      f <- paste0( f, 'else if ( x >= ', funs$max[i], ' ) {\n' )
      if ( funs$c[i] == 0.0 ) {
        f <- paste0( f, '\t\tf <- (', funs$b[i], ')*', funs$max[i], 
                    ' + (', funs$a[i], ')\n\t} ' )
      } else {
        f <- paste0( f, '\t\tf <- (', funs$b[i], ')*exp( -(', funs$c[i], ')*', 
                    funs$max[i], ') + (', funs$a[i], ')\n\t} ' )
      }
      f <- paste0( f, '\n\tf <- max(0.0,f)' )
      f <- paste0( f, '\n\tf <- min(1.0,f)' )
      f <- paste0( f, '\n\treturn(f)\n}' )
      
      strf <- paste0( strf, '\n', f )
      
      eval( parse( text = f ), envir = envir )
      
    }
  }
  rm(i,j,f)
  return( list( funs, strf ) )
}

Read.Utilities <- function( file, script, lines, skip = 2, encoding = 'utf-8' ) {
  .Deprecated(
    new = 'read_utilities',
    msg = 'The function Read.Utilities will be replaced by the function read_utilities',
    old = 'Read.Utilities' )
  return( read_utilities( file, lines, skip, encoding ) )
}
