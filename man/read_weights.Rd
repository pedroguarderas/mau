\name{read_weights}
\alias{read_weights}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
read_weights
}
\description{
Función para leer los pesos del modelo de Logical Decisions en un data frame.
}
\usage{
read_weights(file, cols, encoding = "latin1")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{file}{
  Archivo xls/xslx de entrada que contiene los pesos de Logical Decisions, se debe indicar la path.
}
  \item{cols}{
  Número de columnas a leerse.
}
  \item{encoding}{
  Caracter que representa el encoding del archivo, por defecto 'latin1'.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
La función \code{read_weights} retorna un data frame que contiene los pesos del modelo de Logical Decisions.
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
# Utilice el archivo weights.xls
weights<-read_weights( 'example/weights.xls', 1:64 )
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
