\name{index_weights}
\alias{index_weights}
\title{
index_weights
}
\description{
Función para extraer el valor de los indicadores a partir de un árbol de decisión con pesos relativos.
}
\usage{
index_weights(tree)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{tree}{
  Data frame que contiene la estructura del árbol, con nodos y hojas, del modelo de evaluación en 
  formato \code{graphml}.
}
}
\details{
La función \code{index_weights} establece el valor de los indicadores a partir de los pesos relativos 
del árbol de decisión, 
}
\value{
La función \code{index_weights} retorna un data frame que contiene el valor de los indicadores del modelo 
de decisión.
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
 \code{\link{load_tree}}, \code{\link{write_tree}}, \code{\link{sum_weights}}, \code{\link{sum_weights}}
}
\examples{
# Lectura del archivo 'graphml'
tree<-read.graph( 'example/tree.graphml', format = 'graphml' )

# Asignación de pesos
tree<-sum_weights( tree )

# Reasignación de pesos
tree<-divide_weights( tree )

# Extracción de indicadores
new_weights<-index_weights( tree )
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
