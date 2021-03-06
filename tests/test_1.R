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

