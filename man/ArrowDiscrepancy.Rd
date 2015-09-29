\name{ArrowDiscrepancy}
\alias{ArrowDiscrepancy}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ArrowDiscrepancy
%%  ~~function to do ... ~~
}
\description{
  Función que calcula una discrepancia entre las utilidades de un individuo evaluado con respecto a 
  otros individuos en una matriz "U" (La primera columna de U corresponde al código que identifica a 
  cada individuo evaluado, las restantes corresponden a las utilidades por indicador del modelo de 
  evaluación).
}
\usage{
ArrowDiscrepancy(U)
}
%- maybe also 'usage' fo
\arguments{
  \item{U}{
  Matriz de utilidades.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
 La función \code{ArrowDiscrepancy} retorna el siguiente objeto:

  \item{A}{Matriz comparativa de discrepancias porcentuales de la matriz utilidad U.}
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
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
 # Matriz de utilidades
 utilidades<-data.frame(cod=c('A', 'B', 'C', 'D'), 
                  ind1=c(0.4428345, 0.454545, 0.6, 1),
                  ind2=c(0.7, 0.65, 0.231, 0.5), 
                  ind3=c(0, 0.2696746, 0.6751261, 0.7401660),
                  ind4=c(0.2797259, 0.2981198, 1, 0.1952864))
 
 # Matriz de discrepancias
 ArrowDiscrepancy(utilidades)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
