\name{sum_weights}
\alias{sum_weights}
\title{
sum_weights
}
\description{
Función para asignar pesos a las ramas internas, del árbol de decisión en formato \code{graphml}, 
dados los pesos en las raíces.
}
\usage{
sum_weight(tree)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{tree}{
  Data frame que contiene la estructura del árbol, con nodos y hojas, del modelo de evaluación en 
  formato \code{graphml}.
}
}
\details{
La función \code{sum_weights} establece los pesos en las ramas internas del árbol de decisión, dado 
los pesos en las raíces, mediante la suma de sus pesos.
}
\value{
La función \code{sum_weights} retorna un objeto de tipo \code{igraph} que contiene los pesos en las 
ramas internas del árbol de decisión.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Julio Andrade, Pedro Guarderas, Daniel Lagos, Andrés López, Paúl Recalde, Edison Salazar.
}
\note{
Para utilizar las funciones correctamente, se las debe utilizar en orden:  1.\code{\link{sum_weights}}, 
2.\code{\link{divide_weights}}, 3.\code{\link{index_weights}}.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
 \code{\link{load_tree}}, \code{\link{write_tree}}
}
\examples{
# Lectura del archivo 'graphml'
tree<-read.graph( 'example/tree.graphml', format = 'graphml' )

# Asignación de pesos
tree<-sum_weights( tree )
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
