\name{Función para gráficar las posiciones de las IES sobre una función de utilidad específica}
\alias{fun.util.ies}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Función para gráficar las posiciones de las IES sobre una función de utilidad específica
%%  ~~function to do ... ~~
}
\description{
La función \code{fun.util.ies} permite graficar la posición de las IES sobre una función de utilidad específica. 
}
\usage{
fun.util.ies(data, funs, cod.funcion, size.font, xsize.label, ysize.label, legend.pos, cols, col.line, size.line, xlab.plot, ylab.plot)
}
\arguments{
  \item{data}{
  Data frame que incluye el nombre de las IES, el valor del indicador y el valor de la utilidad de acuerdo a la función de utilidad específica.
}
  \item{funs}{
  Data frame que contiene la función de utilidad específica, su código, sus mínimos y máximos.
}
  \item{cod.funcion}{
  Código de la función de utilidad específica a graficar, proviene del data frame funs.
}
  \item{size.font}{
  Tamaño de la fuente del gráfico.
}
  \item{xsize.label}{
  Tamaño de la fuente en el eje X.
}
  \item{ysize.label}{
  Tamaño de la fuente en el eje Y.
}
  \item{legend.pos}{
  Posición de la leyenda
}
  \item{cols}{
  Colores de las IES en el gráfico.
}
  \item{col.line}{
  Color de la línea de la función de utilidad.
}
  \item{size.line}{
  Tamaño de la línea de la función de utilidad.
}
  \item{xlab.plot}{
  Título del eje X.
}
  \item{ylab.plot}{
  Título del eje Y.
}
}
\details{
}
\value{
La función \code{fun.util.ies} depende de la función interna \code{util.style} que establece estándares para el tema ggplot2.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{Julio Andrade, Pedro Guarderas, Andrés López, Paúl Recalde
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
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
funs$cod<-rep(1:4,each=2)

source('example/funciones_utilidad.R')

# Datos para el ejemplo
data<-data.frame(id.ies=c(3,4,7,8),valor=c(3,6,5,12),
                 nom_imp<-paste('IES','_',LETTERS[1:4],sep=''))
data$utilidad<-sapply(data$valor,FUN=actividades_extension_divulgacion)

data<-data
funs<-funs
cod.funcion<-1
size.font<-22
xsize.label<-20
ysize.label<-20
legend.pos<-c(0.7, 0.35)
# legend.pos<-'right'
cols<-c("saddlebrown","springgreen3",'palevioletred3','paleturquoise3')
col.line<-'cornflowerblue'
size.line<-1.2
xlab.plot<-funs$nom[1]
ylab.plot<-'Utilidad'

# Función para graficar
fun.util.ies(data,funs,cod.funcion,size.font,xsize.label,ysize.label,legend.pos,
                       cols,col.line,size.line,xlab.plot,ylab.plot)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
