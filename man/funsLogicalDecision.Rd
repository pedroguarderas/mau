\name{funsLogicalDecision}
\alias{funsLogicalDecision}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Macro para leer funcione del Logical Decision
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
funsLogicalDecision(data = stop("data not supplied"), file = stop("file not supplied"), script = stop("script not supplied"), case = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
%%     ~~Describe \code{data} here~~
}
  \item{file}{
%%     ~~Describe \code{file} here~~
}
  \item{script}{
%%     ~~Describe \code{script} here~~
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
Felipe Aguirre, Pedro Guarderas
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
macro(data, file, script, case = TRUE)
{
    data <- read.table(file, header = FALSE, sep = "\t", quote = NULL, 
        encoding = "latin1", skip = 5, nrows = 133, allowEscapes = FALSE, 
        dec = ".", fill = TRUE, strinsAsFactors = TRUE)
    data <- data[, !(1:ncol(data) \%in\% c(3, 5, 8))]
    data <- data.frame(data, fun = correct_char2(data, 1))
    colnames(data) <- c("nom", "min", "max", "nivel", "val", 
        "a", "b", "c", "fun")
    if (file.exists(script)) {
        file.remove(script)
    }
    i <- 1
    while (i < nrow(data)) {
        if (data$fun[i] != "") {
            f <- paste(data$fun[i], "<-function(x) { \n\tf<-NULL \n", 
                sep = "")
            j <- i + 1
            while (data$fun[j] == "" && j <= nrow(data)) {
                if (j == i + 1) {
                  f <- paste(f, "\tif ( x >=", data$min[j], " && x <=", 
                    data$max[j], " ) {\n", sep = "")
                }
                else {
                  f <- paste(f, "else if ( x >= ", data$min[j], 
                    " && x < ", data$max[j], " ) {\n", sep = "")
                }
                if (data$c[j] == 0) {
                  f <- paste(f, "\t\tf<-(", data$b[j], ")*x + (", 
                    data$a[j], ")\n\t} ", sep = "")
                }
                else {
                  f <- paste(f, "\t\tf<-(", data$b[j], ")*exp( -(", 
                    data$c[j], ")*x ) + (", data$a[j], ")\n\t}", 
                    sep = "")
                }
                j <- j + 1
            }
            f <- paste(f, "\n\treturn(f)\n}", sep = "")
            write(f, file = script, append = TRUE)
            i <- j
        }
    }
    rm(i, j, f)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
