
<!-- README.md is generated from README.Rmd. Please edit that file -->
Introduction
------------

The MAUT models are defined with aid of utility functions *u*<sub>1</sub>, …, *u*<sub>*n*</sub> which are evaluated over indexes *x*<sub>1</sub>, …, *x*<sub>*n*</sub> and those utilities are aggregated considering additional weights *w*<sub>1</sub>, …, *w*<sub>*n*</sub>, the whole final utility is given by the addition

*u*(*x*<sub>1</sub>, …, *x*<sub>*n*</sub>)=∑<sub>1 ≤ *i* ≤ *n*</sub> *w*<sub>*i*</sub> *u*<sub>*i*</sub> (*x*<sub>*i*</sub>)

With **mau** you can build and test decision models based in Multi Attribute Utility Theory (MAUT). The utilities of any level of the decision model can be easily evaluated.

Installation
------------

To install **mau** you can proceed in the following way making use of the devtools library

``` r
library( devtools )
install_github( "pedroguarderas/mau" )
```

Utility definition
------------------

The utility functions for a MAUT model could be defined in a practical format when the utilities are piecewise defined like constant risk adverse functions, in such case it is only necessary to define the parameters of the function for each part of the domain of definition. This is because the constant risk adversion functions are only of the following form *u*(*x*)=*a* ⋅ *x* + *b* or *u*(*x*)=*a* ⋅ *e*<sup>*b* ⋅ *x*</sup> + *c*. Then the format for the piecewise defintion of the utility is

Header

Function name
min1 max1 a1 b1 c1
min2 max2 a2 b2 c2
min3 max3 a3 b3 c3
...
Function name
min1 max1 a1 b1 c1
min2 max2 a2 b2 c2
min3 max3 a3 b3 c3
...

If *c*<sub>*i*</sub> is 0 then the utility is linear, otherwise is an exponential function. For example:

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

Main example
------------

In the sources below is developed a complete example of a MAUT model, the package **mau** is employed to load utilities defined in the file `utilities.txt`, automatically an script with utilies is built and saved in the local working directory, after that with `Eval.Utilities` every function is evaluated over the columns of the index table, the names for utilities were previously standarized with `Stand.String`. With another file `tree.csv` the decision tree associated to the MAUT model is built and every weight and relative weight assigned with the `Make.Decision.Tree` function, in addition the whole model with utilies of every criteria is obtained with `Compute.Model`. The simulation of constrained weights is made with `Sim.Const.Weights`, the result could be employed for a sensitivy test of the decision model regarding concentrated weights variation.

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
sheetIndex<-1
cols<-1:5
rows<-c(1,8)
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
