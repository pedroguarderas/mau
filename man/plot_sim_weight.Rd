% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_simulation_weight.R
\name{plot_sim_weight}
\alias{plot_sim_weight}
\title{Plot decision MAUT model with weights simulations}
\usage{
plot_sim_weight(
  S,
  title = "Simulations",
  xlab = "ID",
  ylab = "Utility",
  lines.cols = "blue",
  box.col = "gold",
  box.outlier.col = "darkred",
  utility.col = "darkgreen",
  utility.point.col = "darkgreen",
  text.col = "black"
)
}
\arguments{
\item{S}{first element of the simulation list produced by the function 
\code{\link{sim_weights}}, \code{\link{sim_const_weights}}.}

\item{title}{text for the title plot.}

\item{xlab}{text for x-axis label.}

\item{ylab}{text for y-axis label.}

\item{lines.cols}{the spectrum of colors for the simulation is selected randomly from a base 
color.}

\item{box.col}{color for the boxes.}

\item{box.outlier.col}{color for the outlier points representing the extreme observations in the
boxplot.}

\item{utility.col}{the main utility value is also plotted with this specific color.}

\item{utility.point.col}{the line of main utilities is plotted with points represented with this
color.}

\item{text.col}{color for the text values plotted for each utility.}
}
\value{
ggplot object with the plot of simulations.
}
\description{
Spider plot for the decision model considering the weights simulated with a Dirichlet 
distributions, every simulation is represented with lines, a box plot is included to account the 
behavior of every global utility.
}
\seealso{
\code{\link{sim_const_weights}} \code{\link{sim_weights}}
}
\author{
Pedro Guarderas
}
