\name{monthsbet}
\alias{monthsbet}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
monthsbet
}
\description{
%Cálculo de meses entre dos fechas dadas presentes en columnas de un data.frame
Permite calcular el número de meses que han transcurrido entre dos fechas dadas presentes en columnas de un data frame, donde la fecha final es mayor a la fecha inicial.
}
\usage{
monthsbet(X, cols)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{X}{
  Data frame que contiene las fechas.
}
  \item{cols}{
  %Dos columnas con fechas
  Columnas del data frame, la primera contiene la fecha inicial y la segunda la fecha final,
  con el formato date "\%Y-\%m-\%d".
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
La función \code{monthsbet} retorna un Array que contiene la diferencia en meses entre las fechas final e inicial.

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
month1<-sample(1:12, 30, replace = TRUE)
month2<-sample(1:12, 30, replace = TRUE)
day1<-sample(1:28, 30, replace = TRUE)
day2<-sample(1:28, 30, replace = TRUE)

start_date<-as.Date(paste("2013", month1, day1 ,sep= "-"))
ending_date<-as.Date(paste("2014", month2, day2 ,sep= "-"))

tst<-data.frame(init=start_date, end=ending_date)

# Uso por filas específicas del data frame
monthsbet(tst[2,], c(1,2))
monthsbet(tst[10,], c(1,2))
monthsbet(tst[24,], c(1,2))

# Uso en todas las filas del data frame
apply( tst, 1, FUN = monthsbet, cols = c(1,2) )

tst$num<-apply( tst, 1, FUN = monthsbet, cols = c(1,2) )
tst

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
