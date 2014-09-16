\name{correct_char2}
\alias{correct_char2}
\title{
Corrección de caracteres 2
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Función para corrección de caracteres, está diseñada para eliminar y/o reemplazar pronombres, 
preposiciones comunes, caracteres especiales, espacios.

}
\usage{
correct_char2(data = data.frame,
              cols = columna)
}

%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
  Data frame que contiene el texto a modificar.
}
  \item{cols}{
  Número de columna que contiene el texto a modificar.
}
  \item{chr}{
  Vector nulo de almacenamiento de caracteres por corregir.
}
  \item{rep}{
  Vector nulo de almacenamiento de caracteres de reemplazo.
}
}
\details{
Los pronombres, preposiciones comunes, caracteres especiales, espacios a reemplazar y/o eliminar son:
' y/o ', ' las ', ' el ', ' para ', ' o ', ' en ', ' del ', ' de ', ' la ', ' a ', ' por ', ' y ', 
'/', " ( )*", ' ', 'á', 'é', 'í', 'ó', 'ú', '-', '\\(', '\\)', '( )$', '_(_)*'.
}
\value{
La función \code{correct_char2} retorna el data frame modificado de acuerdo a la columna y opción 
seleccionada.
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

\code{\link{owngsub}}, \code{\link{correct_char}}
}
\examples{
tst<-data.frame(a=c('HÚàn', 'Marìa','juLIO Pér', 'Stevén'), b=1:4)
cols<-1

correct_char2(tst, 1)
tst[,1]<-correct_char2(tst,1)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
