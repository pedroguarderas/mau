\name{correct_char}
\alias{correct_char}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Corrección de caracteres.
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Función para corrección de caracteres, está diseñada para eliminar vocales con tíldes y colocar 
texto en minúsculas o mayúsculas.

}
\usage{
%- maybe also 'usage' for other objects documented here.
correct_char( data = stop("data not supplied"), 
              cols = stop("cols not supplied"), 
              case = TRUE )
}

\arguments{
  \item{data}{
%%     ~~Describe \code{data} here~~
Data frame que contiene el texto a modificar.

}
  \item{cols}{
%%     ~~Describe \code{cols} here~~
Número de columna que contiene el texto a modificar.
}
  \item{case}{
%%     ~~Describe \code{case} here~~
Opción TRUE o FALSE, si es TRUE elimina vocales con tíldes y coloca el texto en mayúsculas, si es FALSE
elimina vocales con tíldes y coloca el texto en minúsculas.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
\code{correct_char} retorna el data frame modificado de acuerdo a la columna y opción seleccionada.
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
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
\code{\link{owngsub}}, \code{\link{correct_char2}}
}
\examples{
tst<-data.frame(a=c('HÚàn', 'Marìa','juLIO Pér', 'Stevén'), b=1:4)
correct_char(tst, 1)

tst<-data.frame(a=c('HÚàn', 'Marìa','juLIO Pér', 'Stevén'), b=1:4)
correct_char(tst, 1, FALSE)

}
%##---- Should be DIRECTLY executable !! ----
%##-- ==>  Define data, use random,
%##--  or do  help(data=index)  for the standard data sets.

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }
