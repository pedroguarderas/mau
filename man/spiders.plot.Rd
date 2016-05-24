\name{Función para crear gráficos de Araña}
\alias{spiders.plot}
\title{Función para crear gráficos de Araña
}
\description{
La función \code{spiders.plot} permite realizar gráficos de araña.
}
\usage{
spiders.plot(data, line_type, title, labels, label_size, label_pos, line.col, pos_scale, title_leg, col_leg, ncol_leg, pos_legx, pos_legy, line_sd, line_width, lty_legend, start)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
  Data frame que contiene la información para graficar.
}
  \item{line_type}{
  Permite que el gráfico de araña muestre los resultados mediante líneas radiales (r), polígonos (p), símbolos (s) o la combinación de estos.
}
  \item{title}{
  Título del gráfico de araña.
}
  \item{labels}{
  Nombre de los ejes del gráfico de araña, son los nombres de las columnas del data frame.
}
  \item{label_size}{
  Escala del texto en el gráfico, proviene del parámetro cex de \code{radial.plot}.
}
  \item{label_pos}{
  Posición de las etiquetas en los vértices del gráfico de araña.
}
  \item{line.col}{
  Colores de las líneas resultado del data frame.
}
  \item{pos_scale}{
  Posición de las etiquetas de las líneas radiales, pueden ser de 1 a 4.
}
  \item{title_leg}{
  Etiquetas de las líneas resultado del data frame en la leyenda.
}
  \item{col_leg}{
  Colores de las líneas resultado en la leyenda.
}
  \item{ncol_leg}{
  El número de columnas en el cual ubicar la leyenda.
}
  \item{pos_legx}{
  Posición de la leyenda en el eje X.
}
  \item{pos_legy}{
  Posición de la leyenda en el eje Y.
}
  \item{line_sd}{
  Estilos de las línea resultado.
}
  \item{line_width}{
  Ancho de las líneas resultados.
}
  \item{lty_legend}{
  Estilos de las líneas resultado en la leyenda.
}
  \item{start}{
  Giro de los ejes del gráfico de araña.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
Revisar las funciones \code{radial.plot}, \code{radial.plot.labels} de la librería \code{plotrix}.
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
# Datos
nom<- c('Academia', 'Estudiantes', 'Investigación', 'Organización', 'Recursos e infraestructura', 
        'Vinculación con la sociedad')
pro<- c(0.3471814, 0.5868377, 0.2337332, 0.5043269, 0.8343328, 0.474359)
est<- c(0.6000000, 0.6000000, 0.6000000, 0.6000000, 0.6000000, 0.600000)

data<-data.frame(rbind(pro, est))
names(data)<-nom

# Gráfico
data<-data
cols<-colnames( data )
# cols<-cols[order(cols)]
start<-1.5
line_type<-'p'
line_sd<-c(5,1)
line_width<-c(3,3)
lty_legend<-c(5,1)
title<-"Criterios"
labels<-cols
label_size<-1.5
label_pos<-1.1
pos_scale<-2
line.col=c('darkorchid','deepskyblue3')
title_leg<-c('Promedio','Estándar de Calidad 0.6')
col_leg<-line.col
ncol_leg<-1
pos_legx<-1
pos_legy<--0.75

# Función para graficar
spiders.plot( data, line_type, title, labels, label_size, label_pos, line.col, pos_scale, title_leg,
               col_leg, ncol_leg, pos_legx, pos_legy, line_sd, line_width, lty_legend, start)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
