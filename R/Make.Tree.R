# Create decision tree -----------------------------------------------------------------------------
#' @title Evaluate utilities
#' @description Create decision tree for MAUT models exporting to an igraph object.
#' @param tree.data data.table with decision tree information.
#' @return igraph object containing the graph of the decision tree.
#' @author Pedro Guarderas, Andrés Lopez
#' \email{pedro.felipe.guarderas@@gmail.com}
#' @seealso \code{\link{Read.Tree}}
#' @details With the tree information loaded by the \code{\link{Read.Tree}} the decision tree
#' could be represented like an igraph object.
#' @examples
#' library( data.table )
#' library( igraph )
#' file<-system.file("extdata", "tree.csv", package = "mau" )
#' tree.data<-Read.Tree( file, skip = 0, nrows = 8 )
#' tree<-Make.Decision.Tree( tree.data )
#' plot( tree )
#' @importFrom igraph make_empty_graph add_vertices add_edges V neighborhood %>%
#' @export
Make.Decision.Tree<-function( tree.data ) {
  
  tree<-make_empty_graph( directed = TRUE )
  for ( i in 1:nrow( tree.data ) ) { # i<-1
    A<-list()
    if ( is.na( tree.data$cod[i] ) ) {
      A<-list( 'id' = tree.data$id[i],
               'code' = 0,
               'name' = tree.data$name[i],
               'color' = 'dodgerblue1',
               'weight' = tree.data$weight[i],
               'rweight' = 0.0,
               'leaf' = 0,
               'function' = '',
               'deep' = 0,
               'size' = 21 )
    } else {
      A<-list( 'id' = tree.data$id[i],
               'code' = tree.data$cod[i],
               'name' = tree.data$name[i],
               'color' = 'orange',
               'weight' = tree.data$weight[i],
               'rweight' = 0.0,
               'leaf' = 1,
               'function' = tree.data$funct[i],
               'deep' = 0,
               'size' = 20 )
      
    }
    tree<-tree %>% add_vertices( 1, attr = A )  
    if ( tree.data$id[i] != tree.data$parent[i] & !is.na( tree.data$parent[i] ) ) {
      tree<-tree %>% add_edges( c( tree.data$name.parent[i], tree.data$name[i] ), color = "black" )
    }
  }
  
  tree<-Sum.Weights( tree ) # weights
  tree<-Divide.Weights( tree ) # relative weights
  tree<-Deep.Compute( tree )
  
  return( tree )
}

# Sum weights --------------------------------------------------------------------------------------
#' @title Sum weights for internal nodes
#' @description The weights of the internal nodes has to be computed first is necessary to add each
#' weights of the leaves.
#' @param tree igraph object representing the tree
#' @return igraph object updated
#' @author Pedro Guarderas, Andrés Lopez
#' @seealso \code{\link{Read.Tree}}
#' @importFrom igraph V ego
#' @export
Sum.Weights<-function( tree ) {
  with( tree, {
  childs<-NULL
  noleaves<-which( V(tree)$leaf == 0 )
  leaves<-which( V(tree)$leaf == 1 )
  
  for ( i in noleaves ) { # i<-leaves[1]
    childs<-unlist( ego( tree, 100, V(tree)[i], mode = 'out' ) )
    childs<-childs[ childs %in% leaves ]
    V( tree )[i]$weight<-sum( V( tree )[ childs ]$weight )
  }
  return( tree )
  })
}

# Compute relative weights -------------------------------------------------------------------------
#' @title Divide weights of internal nodes
#' @description After the addition of weights for internal nodes the final weights have to be 
#' computed dividing by the total weight of each parent.
#' @param tree igraph object representing the tree
#' @return igraph object updated
#' @author Pedro Guarderas, Andrés Lopez
#' @seealso \code{\link{Read.Tree}}
#' @importFrom igraph V ego neighborhood
#' @export
Divide.Weights<-function( tree ) {
  with( tree, {
  noleaves<-which( V(tree)$leaf == 0 )
  leaves<-which( V(tree)$leaf == 1 )
  for ( i in 1:length( V(tree) ) ) { # i<-leaves[9]
    parent<-unlist( ego( tree, 1, V(tree)[i], mode = 'in' ) )
    parent<-parent[ parent != i ]
    childs<-unlist( neighborhood( tree, 100, V(tree)[i], mode = 'out' ) )
    childs<-childs[ childs %in% leaves ]
    if ( length( parent ) == 1 ) {
      pchilds<-unlist( ego( tree, 100, V(tree)[parent], mode = 'out' ) )
      pchilds<-pchilds[ pchilds %in% leaves ]
      V( tree )[i]$rweight<-sum( V(tree)[childs]$weight ) / sum( V(tree)[pchilds]$weight )
    } else if ( length( parent ) == 0 ) {
      V( tree )[i]$rweight<-1.0
    }
  }
  return( tree )
  })
}

# Assign deep identifier ---------------------------------------------------------------------------
#' @title Compute the deep position of every node
#' @description For the computation of the complete decision model is necessary to establish the 
#' deep position of every node.
#' @param tree igraph object representing the tree
#' @return igraph object updated
#' @author Pedro Guarderas, Andrés Lopez
#' @seealso \code{\link{Read.Tree}}
#' @importFrom igraph V ego neighborhood
#' @export
Deep.Compute<-function( tree ) {
  with( tree, {
  parent<-NULL
  childs<-NULL
  nodes<-which( V(tree)$leaf == 0 )
  for( i in nodes ) {
    parent<-unlist( ego( tree, 1, V(tree)[i], mode = 'in' ) )
    parent<-parent[ !( parent %in% i ) ]
    if ( length( parent ) == 0 ) {
      break
    }
  }
  
  parent<-i
  deep<-0
  while ( length( parent ) > 0 ) {
    V(tree)[parent]$deep<-deep  
    deep<-deep + 1
    childs<-unique( unlist( neighborhood( tree, 1, V(tree)[parent], mode = 'out' ) ) )
    childs<-childs[ !( childs %in% parent ) ]
    parent<-childs
  }
  
  return( tree )
  })
}

# Get index values ---------------------------------------------------------------------------------
#' @title Compute leaves weights
#' @description The computation of weights could be determined in an inverse processes given the
#' internal weights.
#' @param tree igraph object representing the tree
#' @return igraph object updated
#' @author Pedro Guarderas, Andrés Lopez
#' @seealso \code{\link{Read.Tree}}
#' @importFrom igraph V ego neighborhood
#' @export
Index.Weights<-function( tree ) {
  with( tree, {
  leaves<-which( V(tree)$leaf == 1 )
  weights<-data.frame()
  for ( i in leaves ) {
    parents<-unlist( neighborhood( tree, 5, V(tree)[i], mode = 'in' ) )
    weights<-rbind( weights, c( V(tree)[i]$codigo, prod( V(tree)[parents]$weight ) ) )
  }
  colnames( weights )<-c( 'cod', 'weight' )
  weights<-weights[ order( weights$cod ), ]
  rownames( weights )<-NULL
  return( weights )
  })
}
