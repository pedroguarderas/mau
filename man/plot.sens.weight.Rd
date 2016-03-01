\name{plot.sens.weight}
\alias{plot.sens.weight}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
plot.sens.weight(V, indicadores.nombres, xlab = "IES")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{V}{
%%     ~~Describe \code{V} here~~
}
  \item{indicadores.nombres}{
%%     ~~Describe \code{indicadores.nombres} here~~
}
  \item{xlab}{
%%     ~~Describe \code{xlab} here~~
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
function (V, indicadores.nombres, xlab = "IES") 
{
    S <- copy(V)
    parplot <- S[, list(q25 = quantile(sens, probs = 0.25)[[1]], 
        q50 = quantile(sens, probs = 0.5)[[1]], q75 = quantile(sens, 
            probs = 0.75)[[1]], max = max(sens)), by = list(ind)]
    ind.nom <- data.table(indicadores.nombres)
    setnames(ind.nom, "id", "ind")
    ind.nom <- ind.nom[, list(ind, nom)]
    parplot <- merge(parplot, ind.nom, by = "ind")
    parplot <- parplot[with(parplot, order(q50)), ]
    parplot[, `:=`(ind, factor(ind, levels = parplot$ind))]
    S[, `:=`(ind, factor(ind, levels = parplot$ind))]
    MAX <- round(max(parplot$max), 2)
    ylabs <- paste(formatC(100 * seq(0, 1, length.out = 21), 
        digits = 1, format = "f", decimal.mark = ","), "\%", sep = "")
    sim_plot <- ggplot(S) + aes(x = ind, y = sens) + geom_boxplot(notch = FALSE, 
        show_guide = FALSE, fill = "gold", alpha = 0.4, outlier.colour = "darkred") + 
        xlab("Indicadores") + ylab("Sensitividad") + coord_flip() + 
        theme_minimal() + scale_x_discrete(labels = parplot$nom) + 
        scale_y_continuous(breaks = seq(0, 1, length.out = 21), 
            labels = ylabs, limits = c(0, 1)) + theme(legend.position = "none") + 
        ggtitle("Sensitividad debida a los pesos")
    return(sim_plot)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
