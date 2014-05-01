\name{correct_char2}
\alias{correct_char2}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Correción de caracteres
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
correct_char2(data, cols, chr = NULL, rep = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
%%     ~~Describe \code{data} here~~
}
  \item{cols}{
%%     ~~Describe \code{cols} here~~
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
Julio Andrade, Daniel Lagos, Andrés Lopez, Pedro Guarderas, Edison Salazar
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
function (data, cols, chr = NULL, rep = NULL) 
{
    out <- data[, cols]
    if (length(chr) == 0 || length(rep) == 0) {
        chr <- c(" en ", " de ", " la ", " a ", " por ", " y ", 
            "/", " ( )*", " ", "á", "é", "í", "ó", "ú", 
            "-")
        rep <- c(" ", " ", " ", " ", " ", " ", "_", " ", "_", 
            "a", "e", "i", "o", "u", "_")
    }
    if (length(chr) == length(rep)) {
        out <- apply(as.data.frame(out), c(1, 2), FUN = tolower)
        out <- apply(as.data.frame(out), c(1, 2), FUN = str_trim)
        for (i in 1:length(chr)) {
            out <- apply(as.data.frame(out), c(1, 2), FUN = owngsub, 
                pattern = chr[i], replacement = rep[i])
        }
    }
    out <- data.frame(out)
    return(out)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
