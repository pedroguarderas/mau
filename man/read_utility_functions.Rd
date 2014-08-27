\name{read_utility_functions}
\alias{read_utility_functions}
\title{
read_utility_function
}
\description{
  Función para construir funciones en R a partir de funciones de utilidad definidas en Logical 
  Decisions.
}
\usage{
read_utility_functions( file, script, nr, skip )
}
\arguments{
  \item{file}{ 
  Archivo txt generado por Logical Decisions con las funciones de decisión, se debe indicar la path 
  del archivo. }
  \item{script}{ Script R de salida en donde se guardan las funciones de decisión, se debe indicar 
  la path del archivo. }
  \item{nr}{ Número de filas a leerse del archivo txt. }
  \item{skip}{ Número de filas que se omiten. }
}
\details{
}
\value{ La función \code{read_utility_functions} no tiene un valor de retorno, genera un script R 
con funciones de decisión definidas dentro la path especificada por la variable \code{script}. }
\references{
}
\author{
Julio Andrade, Pedro Guarderas, Daniel Lagos, Andrés López, Paúl Recalde, Edison Salazar.
}
\note{
}

\seealso{
}
\examples{
# Utilice el archivo funciones_utilidad.txt
file<-'example/funciones_utilidad.txt'
sheet<-'example/funciones_utilidad.R'
nr<-15
skip<-5

read_utility_functions( file, sheet, nr, skip )
# Revise el archivo funciones_utilidad.R
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }