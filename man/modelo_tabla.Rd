\name{modelo_tabla}
\alias{modelo_tabla}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
Función que cálcula el valor de utilidad de los criterios y subcriterios de un modelo MAUT a partir 
de un árbol tree.graphml y las utilidades correspondientes a los indicadores del modelo.
}
\usage{
modelo_tabla(tree, utilidades)
}
\arguments{
  \item{tree}{
 Un data.frame utilidades que contiene las utilidades de para las diferentes alternativas de modelo
 MAUT y el árbol del formato .graphml.
}
  \item{utilidades}{
%%     ~~Describe \code{utilidades} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
La función \code{modelo_tabla} donde retorna un data.frame llamado modelo_tabla que contiene las
utilidades de los criterios y subcriterios del modelo MAUT. 
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
