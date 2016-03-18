\name{modelo_tabla}
\alias{modelo_tabla}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
modelo_tabla(tree, utilidades)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{tree}{
%%     ~~Describe \code{tree} here~~
}
  \item{utilidades}{
%%     ~~Describe \code{utilidades} here~~
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
function (tree, utilidades) 
{
    model <- eval_criteria(tree, W[, 1], utilidades, FALSE, TRUE, 
        TRUE)
    model_rescale <- eval_criteria(tree, W[, 1], utilidades, 
        TRUE, TRUE, TRUE)
    tip <- c("RAIZ", "CRITERIO", "SUBCRITERIO", "UTILIDAD")
    tabla_objetivos <- data.frame(variable = V(tree)[V(tree)$leaf == 
        1]$id, nom = V(tree)[V(tree)$leaf == 1]$name, weight = V(tree)[V(tree)$leaf == 
        1]$weight, rweight = V(tree)[V(tree)$leaf == 1]$rweight, 
        tipo = tip[4])
    for (i in 0:(length(tip) - 2)) {
        tabla_objetivos <- rbind(tabla_objetivos, data.frame(variable = V(tree)[V(tree)$deep == 
            i & V(tree)$leaf == 0]$id, nom = V(tree)[V(tree)$deep == 
            i & V(tree)$leaf == 0]$name, weight = V(tree)[V(tree)$deep == 
            i & V(tree)$leaf == 0]$weight, rweight = V(tree)[V(tree)$deep == 
            i & V(tree)$leaf == 0]$rweight, tipo = tip[i + 1]))
    }
    modelo_tabla <- melt(model, id.vars = "cod")
    modelo_tabla_rescaled <- melt(model_rescale, id.vars = "cod")
    names(modelo_tabla_rescaled) <- c("cod", "variable", "val_res")
    modelo_tabla <- merge(modelo_tabla, modelo_tabla_rescaled, 
        by = c("cod", "variable"))
    modelo_tabla <- merge(modelo_tabla, tabla_objetivos, by.x = "variable", 
        by.y = "variable")
    return(modelo_tabla)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
