# Index --------------------------------------------------------------------------------------------
index<-data.table( cod = paste( 'A', 1:10, sep = '' ), 
                   i1 = c( 0.3428570, 1, 1, 1, 1, 0.2, 0.7, 0.5, 0.11, 0.8 ),
                   i2 = c( 0.5, 0.5, 1, 0.5, 0.3, 0.1, 0.4, 0.13, 1, 0.74 ), 
                   i3 = c( 0.5, 1.0, 0.75, 0.25, 0.1, 0.38, 0.57, 0.97, 0.3, 0.76 ),
                   i4 = c( 0, 0.2696746, 0.6751261, 0.7401660, 0.84, 0.85, 0.74, 0.65, 0.37, 0.92 ) )

# Loading utilities --------------------------------------------------------------------------------
file<-'example/utilities.txt'
script<-'utilities.R'
lines<-17
skip<-2
encoding<-'utf-8'
functions<-Read.Utilities( file, script, lines, skip, encoding )
source( 'utilities.R' )

# Index positions
columns<-c( 2, 3, 4, 5 )

# Associated names of functions
functions<-sapply( c( 'Project', 'Self implementation', 'External and local relations', 
                      'Scope of capabilities' ),
                   FUN = Stand.String )
names( functions )<-NULL

# Evaluation of utilities
utilities<-Eval.Utilities( index, columns, functions )

# Make tree ----------------------------------------------------------------------------------------
file<-'example/tree.xls'
sheetIndex<-1
cols<-1:5
rows<-c(1,8)
tree.data<-Read.Tree( file, sheetIndex, cols, rows )
tree<-Make.Decision.Tree( tree.data )
tkplot( tree )

# Compute the decision model
weights<-tree.data[ !is.na( weight ) ]$weight
model<-Compute.Model( tree, utilities, weights )

n<-1000
alpha<-c( 0.2, 0.5, 0.1, 0.2 )
constraints<-list( list( c(1,2), 0.7 ), 
                   list( c(3,4), 0.3 ) )
S<-Sim.Const.Weights( n, utilities, alpha, constraints )
plot.S<-Plot.Simulation.Weight( S$simulation, title = 'Simulations', xlab = 'ID', ylab = 'Utility' ) 
plot( plot.S )
