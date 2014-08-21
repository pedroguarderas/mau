\name{sim_eval}
\alias{sim_eval}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
sim_eval
}
\description{
Macro para simular evaluaciones con distribuciones de Dirichlet.
}
\usage{
sim_eval(N = 1, W = stop("W not supplied"), 
         S = stop("S not supplied"), 
         F = stop("F not supplied"), 
         p = stop("p not supplied"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{N}{
  Número de simulaciones.
}
  \item{W}{
  Pesos simulados.
}
  \item{S}{
  Simulaciones.
}
  \item{F}{
  Matriz de indicadores evaluados.
}
  \item{p}{
  Vector de pesos extraídos de Logical Decisions.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~

}
\value{
sim_eval retorna los siguientes objetos:

\item{S}{Data frame de Simulaciones}

\item{W}{Matriz de Pesos simulados}

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
# Matriz de indicadores
indic<-data.frame(cod=c('A', 'B', 'C', 'D'), 
                  ind1=c(0.3428570, 1, 1, 1),
                  ind2=c(0.5, 0.5, 1, 0.5), 
                  ind3=c(0, 0.2696746, 0.6751261, 0.7401660),
                  ind3=c(0.2797259, 0.2981198, 1, 0.1952864))

# Matriz de pesos
rank<-data.frame(cod=c('A', 'B', 'C', 'D'), 
                 weigth=c(0.0376573868, 0.0264447427, 0.0240244958, 0.0179347528))

N<-10
F<-indic
p<-rank$weigth

sim_eval( N, indic, p )

%## The function is currently defined as
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
