\name{plotsaltos}
\alias{plotsaltos}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
plotsaltos
}
\description{
 Función que actua sobre la matriz de saltos generada por \code{\link{analisis_salto}} y genera
 un plot donde se puede apreciar los saltos que realiza por cada simulación.
}
\usage{
plotsaltos(D)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{D}{
   Matriz de simulaciones de individuos evaluados.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
 Genera un boxplot.
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
 \code{\link{analisis_salto}}
}
\examples{
# Matriz de simulaciones
S <- data.frame(cbind(c('A','B','C'),
                      c(0.7556,0.86783,0.7300),
                      c(0.72952,0.83592,0.7488),
                      c(0.895,0.856,0.758)))

colnames(S)<-c('cod','S1','S2','S3')

#Matriz de saltos
D<-analisis_salto( S )

plotsaltos()

}

