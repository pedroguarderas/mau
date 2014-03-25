\name{correct_char}
\alias{correct_char}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Correción de caracteres
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
correct_char(data = stop("data not supplied"), cols = stop("cols not supplied"), case = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
%%     ~~Describe \code{data} here~~
}
  \item{cols}{
%%     ~~Describe \code{cols} here~~
}
  \item{case}{
%%     ~~Describe \code{case} here~~
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
macro(data, cols, case = TRUE)
{
    data[, cols] <- data.frame(apply(as.data.frame(data[, cols]), 
        c(1, 2), FUN = owngsub, pattern = " ( )*", replacement = " "))
    if (case) {
        data[, cols] <- data.frame(apply(as.data.frame(data[, 
            cols]), c(1, 2), FUN = toupper))
    }
    else {
        data[, cols] <- data.frame(apply(as.data.frame(data[, 
            cols]), c(1, 2), FUN = tolower))
    }
    data[, cols] <- data.frame(apply(as.data.frame(data[, cols]), 
        c(1, 2), FUN = owngsub, pattern = "[Á|À]", replacement = "A"))
    data[, cols] <- data.frame(apply(as.data.frame(data[, cols]), 
        c(1, 2), FUN = owngsub, pattern = "[É|È]", replacement = "E"))
    data[, cols] <- data.frame(apply(as.data.frame(data[, cols]), 
        c(1, 2), FUN = owngsub, pattern = "[Í|Ì]", replacement = "I"))
    data[, cols] <- data.frame(apply(as.data.frame(data[, cols]), 
        c(1, 2), FUN = owngsub, pattern = "[Ò|Ó]", replacement = "O"))
    data[, cols] <- data.frame(apply(as.data.frame(data[, cols]), 
        c(1, 2), FUN = owngsub, pattern = "[Ù|Ú]", replacement = "U"))
    data[, cols] <- data.frame(apply(as.data.frame(data[, cols]), 
        c(1, 2), FUN = str_trim))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
