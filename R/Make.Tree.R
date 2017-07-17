# Create decision tree -----------------------------------------------------------------------------
#' @title Evaluate utilities
#' @description Create decision tree for MAUT models exporting to an igraph object
#' @param tree.data data.table with decision tree information
#' @return igraph object
#' @details Details
#' @author Pedro Guarderas, Andrés Lopez
#' @seealso \code{\link{Read.Tree}}
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
  
# Sum weights --------------------------------------------------------------------------------------
  # This function assign weights to the interal nodes of the decision tree
  sum_weights<-function( tree ) {
    noleaves<-which( V(tree)$leaf == 0 )
    leaves<-which( V(tree)$leaf == 1 )
    for ( i in noleaves ) { # i<-leaves[1]
      childs<-unlist( ego( tree, 100, V(tree)[i], mode = 'out' ) )
      childs<-childs[ childs %in% leaves ]
      V( tree )[i]$weight<-sum( V( tree )[ childs ]$weight )
    }
    return( tree )
  }
  
# Compute relative weights -------------------------------------------------------------------------
  divide_weights<-function( tree ) {
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
  }
  
# Assign deep identifier ---------------------------------------------------------------------------
  deep_compute<-function( tree ) {
    
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
  }
  
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
  
  tree<-sum_weights( tree ) # weights
  tree<-divide_weights( tree ) # relative weights
  tree<-deep_compute( tree )
  
  return( tree )
}

# Get index values ---------------------------------------------------------------------------------
# Given a decision tree only with relative weights, this functions can extract the weights of
# indexes
index_weights<-function( tree ) {
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
}
