\name{analisis_salto}
\alias{analisis_salto}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
analisis_salto
}
\description{
 Función que actua sobre una matriz "S" de simulaciones generada por \code{\link{sim_eval}}, y analiza los saltos de posición de los individuos evaluados con respectro a la simulación inicial (S1). 
}
\usage{
analisis_salto(S = stop("S not supplied"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{S}{
  Matriz de simulaciones para un conjunto de individuos.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
La función \code{analisis_salto} un Array que contiene los saltos de posición por simulación.
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
 \code{plot_saltos}
}
\examples{
#Matriz de simulaciones:
S <- data.frame(cbind(c('A','B','C'),
                      c(0.7556,0.86783,0.7300),
                      c(0.72952,0.83592,0.7488),
                      c(0.895,0.856,0.758)))

colnames(S)<-c('cod','S1','S2','S3')

analisis_salto( S )

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
