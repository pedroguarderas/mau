\name{eval_index}
\alias{eval_index}
\title{
eval_index
}
\description{
Función para evaluar los indicadores de acuerdo a las funciones de utilidad definidas en el modelo.
}
\usage{
eval_index(index, names, colcod = 1, index_sub = 'i', colpos, colfun, envir = .GlobalEnv)
}
\arguments{
 \item{index}{ 
  Data frame con indicadores.
  }
 \item{names}{ 
  Data frame de nombres de los indicadores.
  }
 \item{indexsub}{ 
  Prefijo del nombre de las columnas de los indicadores.
  }
 \item{colcod}{
  Columna de inicio del data frame \code{index} definido previamente.
  }
 \item{colpos}{
  Columna de inicio del data frame \code{names} definido previamente.
  }
 \item{colfun}{
  Columna que indica el nombre interno de las funciones de utilidad.
  }
 \item{coltip}{
  Columna que indica el tipo de variable de los indicadores, cualitativos o cuantitativos.
  }
 \item{envir}{
  Entorno de trabajo global, definido con las funciones de utilidad.
  }
}
\details{

}
\value{
La función \code{eval_index} retorna un data frame que contiene los resultados de los indicadores al ser
evaluados por las funciones de utilidad definidas.
}
\references{

}
\author{
Julio Andrade, Pedro Guarderas, Daniel Lagos, Andrés López, Paúl Recalde, Edison Salazar
}
\note{
}

\seealso{
}
\examples{
# Indicadores
indic<-data.frame(cod=c('A', 'B', 'C', 'D'), 
                  i1=c(0.3428570, 1, 1, 1),
                  i2=c(0.5, 0.5, 1, 0.5), 
                  i3=c(0, 0.2696746, 0.6751261, 0.7401660),
                  i4=c(0.2797259, 0.2981198, 1, 0.1952864))
colnames(indic)

# Nombre de indicadores
nomb_indic<-data.frame(cod=1:4, 
                       nom0=c('PROYECTO INSTITUCIONAL', 'SEGUIMIENTO IMPLEMENTADO',
                              'PROYECTOS DE VINCULACION', 'ACTIVIDADES DE EXTENSION Y DIVULGACION'),
                       nom1=c('Proyecto institucional', 'Seguimiento implementado', 
                              'Proyectos de vinculación', 'Actividades de extensión y divulgación'),
                       nomfun=c('proyecto_institucional', 'seguimiento_implementado', 
                                'proyectos_vinculacion', 'actividades_extension_divulgacion'),
                       tip=c('CUALITATIVA', 'CUALITATIVA', 'CUANTITATIVA', 'CUANTITATIVA'))

# Script R con las funciones de utilidad.
source('example/funciones_utilidad.R')

# Uso de la funcion eval_index
indic_eval<-eval_index( index = indic, names = nomb_indic, indexsub = 'i',
                        colcod = 1, colpos = 1, colfun = 4, coltip = 5 )
}

