% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rating_methods.R
\name{colley_rating}
\alias{colley_rating}
\title{Colley method}
\usage{
colley_rating(n, w, l, t = NULL)
}
\arguments{
\item{n}{symmetric matrix of number of times each player faced another, zero diagonal}

\item{w}{accumulated vector of wins for each player}

\item{l}{accumulated vector of losses for each player}

\item{t}{symmetric matrix of ties, with zero diagonal}
}
\value{
Vector with ratings
}
\description{
Rank computation using the Cooley method with ties if required
}
\examples{
d <- 10
n <- matrix( sample( x = 0:5, size = d * d, replace = TRUE ), d, d )
n <- n + t( n )
diag( n ) <- 0
g <- rowSums( n )
# Number of win matches for each team
w <- sapply( 1:d, FUN = function( i ) sample( x = 1:g[i], size = 1, replace = TRUE ) )
# Number of lost matches for ech team
l <- rowSums( n ) - w
r <- colley_rating( n, w, l )
}
\author{
Pedro Guarderas
\email{pedro.felipe.guarderas@gmail.com}
}
