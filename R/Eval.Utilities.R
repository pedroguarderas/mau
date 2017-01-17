# Función para evaluar indicadores con funciones de utilidad

# Evaluate utility functions -----------------------------------------------------------------------
#' @title Evaluate utilities
#' @description Evaluaction of utilities for a data.frame of indexes
#' @param index data of indexes
#' @param names data with name of indexes
#' @param indexsub prefijo del nombre de las columnas de los indicadores.
#' @param colcod columna de inicio del data frame \code{index} definido previamente.
#' @param colpos columna de inicio del data frame \code{names} definido previamente.
#' @param colfun columna que indica el nombre interno de las funciones de utilidad.
#' @param coltip columna que indica el tipo de variable de los indicadores, cualitativos o 
#' cuantitativos.
#' @param envir entorno de trabajo global, definido con las funciones de utilidad.
#' @return La función \code{eval_index} retorna un data frame que contiene los resultados de los 
#' indicadores al ser evaluados por las funciones de utilidad definidas.
#' @details Details
#' @author Julio Andrade, Pedro Guarderas, Daniel Lagos, Andrés López, Paúl Recalde, Edison Salazar
#' @seealso \code{\link{read_weights}}, \code{\link{eval_index}}
#' @examples
#' # Indicadores
#' indic<-data.frame(cod=c('A', 'B', 'C', 'D'), 
#'                   i1=c(0.3428570, 1, 1, 1),
#'                   i2=c(0.5, 0.5, 1, 0.5), 
#'                   i3=c(0, 0.2696746, 0.6751261, 0.7401660),
#'                   i4=c(0.2797259, 0.2981198, 1, 0.1952864))
#' colnames(indic)
#' 
#' # Nombre de indicadores
#' nomb_indic<-data.frame(cod=1:4, 
#'                     nom0=c('PROYECTO INSTITUCIONAL', 'SEGUIMIENTO IMPLEMENTADO',
#'                            'PROYECTOS DE VINCULACION', 'ACTIVIDADES DE EXTENSION Y DIVULGACION'),
#'                     nom1=c('Proyecto institucional', 'Seguimiento implementado', 
#'                            'Proyectos de vinculación', 'Actividades de extensión y divulgación'),
#'                     nomfun=c('proyecto_institucional', 'seguimiento_implementado', 
#'                              'proyectos_vinculacion', 'actividades_extension_divulgacion'),
#'                     tip=c('CUALITATIVA', 'CUALITATIVA', 'CUANTITATIVA', 'CUANTITATIVA'))
#' 
#' # Script R con las funciones de utilidad.
#' source('example/funciones_utilidad.R')
#' 
#' # Uso de la funcion eval_index
#' indic_eval<-Eval.Utilities( index = indic, names = nomb_indic, indexsub = 'i',
#'                      colcod = 1, colpos = 1, colfun = 4, coltip = 5 )
#' @export
Eval.Utilities<-function( index, names, indexsub, colcod = 1, colpos, colfun, coltip, 
                          envir = .GlobalEnv ) {
  
  data<-data.frame( cod = index[ ,colcod ] )

  for ( i in 1:nrow( names ) ) { # i<-2
    # Se verifica si la función ha sido definida
    if ( names[ i, coltip ] == 'CUANTITATIVA' | names[ i, coltip ] == 'CUANTITATIVO'  ) {
      if ( names[ i, colfun ] %in% ls( envir = envir ) ) {
        col<-paste( indexsub, names[ i, colpos ], sep = '' )
        data<-cbind( data, sapply( index[ , col ], FUN = names[ i, colfun ] ) )
        colnames( data )[ ncol(data) ]<-paste( 'eva_', names[ i, colpos ], sep = '' )
      }
    } else {
      col<-paste( indexsub, names[ i, colpos ], sep = '' )
      data<-cbind( data, index[ , col ] )
      colnames( data )[ ncol(data) ]<-paste( 'eva_', names[ i, colpos ], sep = '' )
    }
  }
  rm( i, col )
  return( data )
}
