\name{func_equity}
\alias{func_equity}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
func_equity
}
\description{
Mide la equidad de las funciones de utilidad, mediante simulaciones que siguen una distribución normal, 
del modelo de evaluación. 
}
\usage{
func_equity(f, n, D, I)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{f}{
  Función de decisión.
}
  \item{n}{
  Cantidad de números aleatorios a emplearse en la simulación.
}
  \item{D}{
  Dominio de la función.
}
  \item{I}{
  Intervalo para la evaluación.
}
}
\details{
La función \code{func_equity} evalúa las simulaciones que siguen una distribución normal en el intervalo 
especificado de acuerdo a las funciones de utilidad del modelo
}
\value{
La función \code{func_equity} retorna el siguiente objeto:

\item{equity}{
Data frame que contiene las simulaciones de las funciones de utilidad evaluadas en el intervalo 
\code{I}.
}
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Julio Andrade, Pedro Guarderas, Daniel Lagos, Andrés López, Paúl Recalde, Edison Salazar.
}
\note{
%%  ~~further notes~~
}

\seealso{
 \code{\link{read_utility_functions}}
}
\examples{
options( stringsAsFactors = FALSE )

# Nombre de indicadores
nomb_indic<-data.frame(cod=1:4, 
                       nom0=c('PROYECTO INSTITUCIONAL', 'SEGUIMIENTO IMPLEMENTADO',
                              'PROYECTOS DE VINCULACION', 'ACTIVIDADES DE EXTENSION Y DIVULGACION'),
                       nom1=c('Proyecto institucional', 'Seguimiento implementado', 
                              'Proyectos de vinculación', 'Actividades de extensión y divulgación'),
                       nomfun=c('proyecto_institucional', 'seguimiento_implementado', 
                                'proyectos_vinculacion', 'actividades_extension_divulgacion'),
                       tip=c('CUALITATIVA', 'CUALITATIVA', 'CUANTITATIVA', 'CUANTITATIVA'))

# Función read_utility_functions
file<-'example/funciones_utilidad.txt'
sheet<-'example/funciones_utilidad.R'
nr<-15
skip<-5

funs<-read_utility_functions( file, sheet, nr, skip )

# Data frame de máximos y mínimos de las funciones de utilidad
dom_func<-merge( aggregate( min ~ fun, data = funs, FUN = min, na.rm = TRUE ),
                aggregate( max ~ fun, data = funs, FUN = max, na.rm = TRUE ),
                by = 'fun', all = FALSE )

# Uso de la función func_equity 
f<-dom_func$fun[1]
n<-1000
D<-c(dom_func$min[1], dom_func$max[1])
I<-seq(0, 1, 0.1)

source('example/funciones_utilidad.R')

# Función func_equity aplicada en todas las funciones de utilidad 
equid<-data.frame()
for ( i in 1:nrow( dom_func ) ) {
  equid<-rbind( equid, func_equity( f, n, D, I ) )
}

equid<-cbind( dom_func$fun, equid )
colnames( equid )<-c( 'fun', paste( 'I', 1:(length(I)-1), sep = '' ) )

# Gráfico de la equidad de las funciones de utilidad
boxplot( equid[,2:11] )
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
