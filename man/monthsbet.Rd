\name{monthsbet}
\alias{monthsbet}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
monthsbet
}
\description{
%Cálculo de meses entre dos fechas dadas presentes en columnas de un data.frame
Permite calcular el número (número continuo) de meses que han transcurrido entre dos fechas dadas
presentes en columnas de un data frame.
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
  con el formato date "\%Y-\%m-\%d"
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

month1 <- sample(1:12, 50, replace = TRUE)
month2 <- sample(1:12, 50, replace = TRUE)
day1 <- sample(1:28, 50, replace = TRUE)
day2 <- sample(1:28, 50, replace = TRUE)

start_date <- paste("2013", month1, day1 ,sep= "-")
ending_date <- paste("2014", month2, day2 ,sep= "-")

start_date <- as.Date(start_date)
ending_date <- as.Date(ending_date)

x <- data.frame(SD = start_date, ED = ending_date)

prev<-data.frame( x[,c(1,2)], prctj_mes = apply( x, 1, FUN = monthsbet, cols = c(1,2) ) )



}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line



