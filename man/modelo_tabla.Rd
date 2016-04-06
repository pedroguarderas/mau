\name{modelo_tabla}
\alias{modelo_tabla}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
 modelo_tabla
}
\description{
Función que cálcula el valor de utilidad de los criterios y subcriterios de un modelo MAUT a partir 
de un árbol tree.graphml y las utilidades correspondientes a los indicadores del modelo.
}
\usage{
modelo_tabla(tree, utilidades)
}
\arguments{
  \item{tree}{
 Un data.frame utilidades que contiene las utilidades de para las diferentes alternativas de modelo
 MAUT y el árbol del formato .graphml.
}
  \item{utilidades}{
%%     ~~Describe \code{utilidades} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
La función \code{modelo_tabla} donde retorna un data.frame llamado modelo_tabla que contiene las
utilidades de los criterios y subcriterios del modelo MAUT. 
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

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
