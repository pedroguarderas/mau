\name{ArrowEquity}
\alias{ArrowEquity}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ArrowEquity
%%  ~~function to do ... ~~
}
\description{
Función que permite determinar la equidad de arrow entre los valores de utilidades de una matriz "U"
(La primera columna de U corresponde al código que identifica a cada individuo evaluado, las restantes 
corresponden a las utilidades por indicador del modelo de evaluación) y un vector de valores de 
ranking "r".
}
\usage{
ArrowEquity(U, r)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{U}{
   Matriz de utilidades de la evaluación.
}
  \item{r}{
   Vector de valores de ranking obtenidos para cada individuo evaluado.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
 La función \code{ArrowEquity} retorna el siguiente objeto:

  \item{a}{Vector de equidades de arrow porcentual por cada utilidad.}
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
# Matriz de utilidades
utilidades<-data.frame(cod=c('A', 'B', 'C', 'D'), 
                  ind1=c(0.4428345, 0.454545, 0.6, 1),
                  ind2=c(0.7, 0.65, 0.231, 0.5), 
                  ind3=c(0, 0.2696746, 0.6751261, 0.7401660),
                  ind4=c(0.2797259, 0.2981198, 1, 0.1952864))

# Matriz de ranking
rank<-data.frame(cod=c('A', 'B', 'C', 'D'), 
                 value=c(0.0376573868, 0.0264447427, 0.0240244958, 0.0179347528))

ArrowEquity(utilidades, rank$weigth)
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
