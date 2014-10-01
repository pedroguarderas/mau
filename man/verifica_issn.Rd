\name{verifica_issn}
\alias{verifica_issn}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
verifica_issn
}
\description{
Función para verificar la validez del código ISSN de las publicaciones científicas.
}
\usage{
verifica_issn(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
  Código ISSN de 8 caracteres, en formato \code{character}.
}
}
\details{
La función \code{verifica_issn} comprueba la validez del código ISSN de las publicaciones científicas.
}
\value{
La función \code{read_weights} retorna un objeto de tipo \code{logical} TRUE o FALSE, indicando si se
cumple o no la validez del código ISSN, respectivamente.
}
\references{
International Standard Serial Number
\link{http://en.wikipedia.org/wiki/International_Standard_Serial_Number}
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
# Códigos ISSN
a<-'1553571X'
b<-'13602276'
c<-'03785958'
d<-'13907867'

# Uso de la función
verifica_issn(a)
verifica_issn(b)
verifica_issn(c)
verifica_issn(d)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
