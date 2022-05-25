
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Introduction

The MAUT decision models are defined with aid of utility functions
![u_1,\\ldots,u_n](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;u_1%2C%5Cldots%2Cu_n "u_1,\ldots,u_n")
which are evaluated over indexes
![x_1,\\ldots,x_n](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;x_1%2C%5Cldots%2Cx_n "x_1,\ldots,x_n")
and those utilities are aggregated considering additional weights
![w_1,\\ldots,w_n](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;w_1%2C%5Cldots%2Cw_n "w_1,\ldots,w_n"),
the whole final utility is given by the sum

![u(x_1,\\ldots,x_n) = \\sum\_{1\\leq i \\leq n}\\, w_i\\, u_i\\ ( x_i )](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;u%28x_1%2C%5Cldots%2Cx_n%29%20%3D%20%5Csum_%7B1%5Cleq%20i%20%5Cleq%20n%7D%5C%2C%20w_i%5C%2C%20u_i%5C%20%28%20x_i%20%29 "u(x_1,\ldots,x_n) = \sum_{1\leq i \leq n}\, w_i\, u_i\ ( x_i )")

With **mau** you can build and test decision models based in Multi
Attribute Utility Theory (MAUT). The utilities of any level of the
decision model can be easily evaluated.

## Installation

To install **mau** you can proceed in the following way making use of
the devtools library

``` r
library( devtools )
install_github( "pedroguarderas/mau" )
```

## Utility definition

The utility functions for a MAUT model could be defined in a practical
format when those are are piecewise defined like constant risk averse
functions, in such case it is only necessary to define the parameters of
the function for each part of the domain of definition. This is because,
the constant risk averse functions are of the form
![u(x) = a \\cdot x + b](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;u%28x%29%20%3D%20a%20%5Ccdot%20x%20%2B%20b "u(x) = a \cdot x + b")
or
![u(x) = a \\cdot e^{b \\cdot x} + c](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;u%28x%29%20%3D%20a%20%5Ccdot%20e%5E%7Bb%20%5Ccdot%20x%7D%20%2B%20c "u(x) = a \cdot e^{b \cdot x} + c").

File format for the piecewise definition of utilities, is specified as
follows.  
\>Header  
\>  
\>Function name  
\>min1 max1 a1 b1 c1  
\>min2 max2 a2 b2 c2  
\>min3 max3 a3 b3 c3  
\>…  
\>Function name  
\>min1 max1 a1 b1 c1  
\>min2 max2 a2 b2 c2  
\>min3 max3 a3 b3 c3  
\>…

If
![c_i](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;c_i "c_i")
is
![0](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;0 "0")
then the utility is linear, otherwise is an exponential function. For
example:

``` r
library( mau )
file<-system.file("extdata", "utilities.txt", package = "mau" )
lines<-readLines( file )
for ( i in 1:length( lines ) ) { 
  cat( lines[i], '\n' )
}
#> Utilities 
#>  
#> Project 
#>  1   2   1.5 -0.5    0 
#>  2   3   1.5 -0.5    0 
#>  
#> Self implementation 
#>  1   2   1.5 -0.5    0 
#>  2   3   1.5 -0.5    0 
#>  
#> External and local relations 
#>  1   10  1   0   0 
#>  0   1   0   1   0 
#>  
#> Scope of capabilities 
#>  6   15  1   0   0 
#>  0   6   1.225   -1.225  0.2824
```

## Main example

In the sources below is developed a complete example of a decision
model, the package **mau** is employed to load utilities defined in the
file `utilities.txt`, provided in the package itself, automatically the
script with utilities is built and saved in the local working directory,
after that with `Eval.Utilities` every function is evaluated over the
columns of the index table, the names for utilities were previously
standardized with `Stand.String`. With another file `tree.csv` the
decision tree associated to the MAUT model is built and every weight and
relative weight assigned with the `Make.Decision.Tree` function, in
addition the whole model with utilities of every criteria is obtained
with `Compute.Model`. The simulation of constrained weights is made with
`Sim.Const.Weights`, the result could be employed for a sensitivity test
of the decision model under a variation of weights.

``` r
# Loading packages --------------------------------------------------------------------------------
library( mau )
library( data.table )
library( igraph )
library( ggplot2 )

# Table of indexes --------------------------------------------------------------------------------
index<-data.table( cod = paste( 'A', 1:10, sep = '' ), 
                   i1 = c( 0.34, 1, 1, 1, 1, 0.2, 0.7, 0.5, 0.11, 0.8 ),
                   i2 = c( 0.5, 0.5, 1, 0.5, 0.3, 0.1, 0.4, 0.13, 1, 0.74 ), 
                   i3 = c( 0.5, 1.0, 0.75, 0.25, 0.1, 0.38, 0.57, 0.97, 0.3, 0.76 ),
                   i4 = c( 0, 0.26, 0.67, 0.74, 0.84, 0.85, 0.74, 0.65, 0.37, 0.92 ) )

# Loading utilities -------------------------------------------------------------------------------
file<-system.file("extdata", "utilities.txt", package = "mau" )
script<-'utilities.R'
lines<-17
skip<-2
encoding<-'utf-8'
functions<-Read.Utilities( file, script, lines, skip, encoding )
source( 'utilities.R' )

# Index positions ---------------------------------------------------------------------------------
columns<-c( 2, 3, 4, 5 )

# Function names
functions<-sapply( c( 'Project', 
                      'Self implementation',
                      'External and local relations', 
                      'Scope of capabilities' ),
                   FUN = Stand.String )
names( functions )<-NULL

# Evaluation of utilities -------------------------------------------------------------------------
utilities<-Eval.Utilities( index, columns, functions )

# Tree creation -----------------------------------------------------------------------------------
file<-system.file("extdata", "tree.csv", package = "mau" )
tree.data<-Read.Tree( file, skip = 0, nrow = 8 )
tree<-Make.Decision.Tree( tree.data )

# Compute the decision model ----------------------------------------------------------------------
weights<-tree.data[ !is.na( weight ) ]$weight
model<-Compute.Model( tree, utilities, weights )

# Weights simulation ------------------------------------------------------------------------------
n<-200
alpha<-c( 0.2, 0.5, 0.1, 0.2 )
constraints<-list( list( c(1,2), 0.7 ), 
                   list( c(3,4), 0.3 ) )
S<-Sim.Const.Weights( n, utilities, alpha, constraints )
plot.S<-Plot.Simulation.Weight( S$simulation, title = 'Simulations', 
                                xlab = 'ID', ylab = 'Utility' ) 
plot( plot.S )
```
