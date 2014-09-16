\name{correct_char}
\alias{correct_char}
\title{
Corrección de caracteres.
}
\description{
Función para corrección de caracteres, está diseñada para eliminar vocales con tíldes y colocar 
texto en minúsculas o mayúsculas.

}
\usage{
correct_char( data = stop("data not supplied"), 
              cols = stop("cols not supplied"), 
              case = TRUE )
}

\arguments{
  \item{data}{
  Data frame que contiene el texto a modificar.

}
  \item{cols}{
  Número de columna que contiene el texto a modificar.
}
  \item{case}{
  Opción TRUE o FALSE, si es TRUE elimina vocales con tíldes y coloca el texto en mayúsculas, si es 
  FALSE elimina vocales con tíldes y coloca el texto en minúsculas.
}
}
\details{

}
\value{
La función \code{correct_char} retorna el data frame modificado de acuerdo a la columna y opción 
seleccionada.
}
\references{

}
\author{
Julio Andrade, Pedro Guarderas, Daniel Lagos, Andrés López, Paúl Recalde, Edison Salazar
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{owngsub}}, \code{\link{correct_char2}}
}
\examples{
tst<-data.frame(a=c('HÚàn', 'Marìa','juLIO Pér', 'Stevén'), b=1:4)
correct_char(tst, 1)

tst<-data.frame(a=c('HÚàn', 'Marìa','juLIO Pér', 'Stevén'), b=1:4)
correct_char(tst, 1, FALSE)

}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }
