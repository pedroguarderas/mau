\name{owngsub}
\alias{owngsub}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
owngsub
}
\description{
Función para reemplazo de un argumento modelo por otro, dentro de un vector de caracteres.
}
\usage{
owngsub(x, pattern, replacement)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
  Vector de caracteres que contiene las coincidencias buscadas.
}
  \item{pattern}{
  Cadena de caracteres que contiene el argumento de coincidencia definida por el usuario.
}
  \item{replacement}{
  Cadena de caracteres reemplazo para el argumento \code{pattern} definido.
}
}
\details{
La función \code{owngsub} utiliza la función \code{gsub} para la búsqueda de coincidencias y reemplazo
de argumentos.
} 
\value{
La función \code{owngsub} retorna un vector de los elementos sustituidos de \code{x}, del mismo 
tamaño y los mismos atributos; los elementos no sustituidos retornan sin cambios. 
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

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
 \code{\link{correct_char}},  \code{\link{correct_char2}}
}
\examples{
data<-c('Dire chlet','MÀry', 'Jhon', 'Sálly')
data1<-data.frame(id=1:4, nom=c('Dire chlet','MÀry', 'Jhon', 'Sálly'))

# Uso en un vector
owngsub( data, pattern = c("re "), replacement = c("ri") )
owngsub( data, pattern = c('ho'), replacement = c('oh') )
data<-owngsub( data, pattern = c("re "), replacement = c("ri") )
data<-owngsub( data, pattern = "[Á|À]|[á|à]", replacement = "a" ) 

# Uso en todas las filas del data frame
data1<-apply(data1, 2, FUN=owngsub, pattern = "[Á|À]|[á|à]", replacement = "a")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
