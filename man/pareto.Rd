\name{pareto}
\alias{pareto}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
pareto(SIM, p, m)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{SIM}{
%%     ~~Describe \code{SIM} here~~
}
  \item{p}{
%%     ~~Describe \code{p} here~~
}
  \item{m}{
%%     ~~Describe \code{m} here~~
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
%%  ~~who you are~~
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
function (SIM, p, m) 
{
    com <- function(x, y, m, a) {
        return(sum(x >= y)/m >= a)
    }
    P <- matrix(0, p, p)
    for (i in 1:p) {
        P[, i] <- apply(SIM, 2, FUN = com, y = SIM[, i], m = m, 
            a = 0.9)
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
