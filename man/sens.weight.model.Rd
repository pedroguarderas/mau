\name{sens.weight.model}
\alias{sens.weight.model}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
sens.weight.model(utilidades, weights)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{utilidades}{
%%     ~~Describe \code{utilidades} here~~
}
  \item{weights}{
%%     ~~Describe \code{weights} here~~
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
function (utilidades, weights) 
{
    n <- ncol(utilidades)
    m <- nrow(utilidades)
    V <- NULL
    for (i in 1:m) {
        s <- vari(utilidades[i, 2:n], weights)/vartot(utilidades[i, 
            2:n], weights)
        V <- rbind(V, data.table(cod = utilidades[i, 1], s))
    }
    setnames(V, 1:n, names(utilidades))
    V <- melt(data = V, id.vars = "cod")
    setnames(V, 1:3, c("cod", "ind", "sens"))
    V[, `:=`(ind, as.numeric(gsub("eva_", "", ind)))]
    V <- V[with(V, order(ind, cod)), ]
    return(V)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
