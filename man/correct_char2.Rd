\name{correct_char2}
\alias{correct_char2}
%- Also NEED an '\alias' for EACH other topic documented here.
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
%%     ~~Describe \code{data} here~~
Data frame que contiene el texto a modificar.
}
  \item{cols}{
%%     ~~Describe \code{cols} here~~
Número de columna que contiene el texto a modificar.
}
  \item{chr}{
%%     ~~Describe \code{chr} here~~
}
  \item{rep}{
%%     ~~Describe \code{rep} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
Los pronombres, preposiciones comunes, caracteres especiales, espacios a reemplazar y/o eliminar son:
 ' y/o ', ' las ', ' el ', ' para ', ' o ', ' en ', ' del ', ' de ', ' la ', ' a ', 
            ' por ', ' y ', '/', " ( )*", ' ', 'á', 'é', 'í', 'ó', 'ú', '-', '\\(', '\\)', '( )$', 
            '_(_)*'
}
\value{
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
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
