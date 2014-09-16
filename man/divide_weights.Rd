\name{divide_weights}
\alias{divide_weights}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
divide_weights
}
\description{
Función para reasignar los pesos a las raíces, del árbol de decisión en formato \code{graphml}, 
dados los pesos en las ramas.
}
\usage{
divide_weights( tree )
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{tree}{
  Data frame que contiene la estructura del árbol, con nodos y hojas, del modelo de evaluación en 
  formato \code{graphml}.
}
}
\details{
La función \code{divide_weights} reestablece los pesos en las raíces del árbol de decisión, 
dado los pesos en las ramas, mediante la división entre los pesos de las raíces y la correspondiente 
rama.
}
\value{
La función \code{divide_weights} retorna un objeto de tipo \code{igraph} que contiene los pesos en las 
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
 \code{\link{load_tree}}, \code{\link{write_tree}}, \code{\link{sum_weights}}
}
\examples{
# Lectura del archivo 'graphml'
tree<-read.graph( 'example/tree.graphml', format = 'graphml' )

# Asignación de pesos
tree<-sum_weights( tree )

# Reasignación de pesos
tree<-divide_weights( tree )
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
