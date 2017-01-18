#___________________________________________________________________________________________________
Make.Decision.Tree<-function( tree.data ) {
  tree<-make_empty_graph( directed = TRUE )
  for ( i in 1:nrow( tree.data ) ) { # i<-1
    A<-list()
    if ( is.na( tree.data$cod[i] ) ) {
      A<-list( 'id' = tree.data$id[i],
               'codigo' = 0,
               'name' = tree.data$nom[i],
               'color' = 'dodgerblue1',
               'weight' = tree.data$weight[i],
               'rweight' = 0.0,
               'leaf' = 0,
               'function' = '',
               'deep' = 0,
               'size' = 21 )
    } else {
      A<-list( 'id' = tree.data$id[i],
               'codigo' = tree.data$cod[i],
               'name' = tree.data$nom[i],
               'color' = 'orange',
               'weight' = tree.data$weight[i],
               'rweight' = 0.0,
               'leaf' = 1,
               'function' = tree.data$'function'[i],
               'deep' = 0,
               'size' = 20 )
      
    }
    tree<-tree %>% add_vertices( 1, attr = A )  
    if ( tree.data$id[i] != tree.data$id_parent[i] & !is.na( tree.data$id_parent[i] ) ) {
      tree<-tree %>% add_edges( c( tree.data$parent[i], tree.data$nom[i] ), color = "black" )
    }
  }
  
  tree<-sum_weights( tree ) # pesos absolutos
  tree<-divide_weights( tree ) #pesos relativos
  tree<-deep_compute( tree )
  
  return( tree )
}

#___________________________________________________________________________________________________
# Función para asignar pesos a las ramas internas dados los pesos en las raíces
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
  while ( length(parent) > 0 ) {
    V(tree)[parent]$deep<-deep  
    deep<-deep + 1
    childs<-unique( unlist( neighborhood( tree, 1, V(tree)[parent], mode = 'out' ) ) )
    childs<-childs[ !( childs %in% parent ) ]
    parent<-childs
  }
  
  return( tree )
}

#___________________________________________________________________________________________________
# Extrae valor de indicadores a partir de un árbol con pesos relativos
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