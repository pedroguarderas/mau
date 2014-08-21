\name{AHPmatrix}
\alias{AHPmatrix}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
AHPmatrix
}
\description{
Función para normalizar matrices cuadradas triangulares superiores positivas, utilizada para elaborar
matrices de comparación en el Analytic Hierarchy Process.

}
\usage{
AHPmatrix(M)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{M}{
Matriz cuadrada triangular superior dónde las coordenadas son positivas y definen la relación 
relevancia entre los criterios que represente cada fila y columna.
%%     ~~Describe \code{M} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
AHPmatrix retorna los siguientes objetos:

\item{rowMean}{ Vector promedio de filas de la matriz. }
\item{ahp}{ Matriz normalizada. }
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Julio Andrade, Pedro Guarderas, Daniel Lagos, Andrés López, Paúl Recalde, Edison Salazar
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
m1<-matrix( 1:20, 5, 5 )
m1[ lower.tri( m1 ) ]<-0
ahp<-AHPmatrix(m1)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
