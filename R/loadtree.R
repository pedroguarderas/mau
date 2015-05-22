#___________________________________________________________________________________________________
# Script diseñado para carga arboles de decisión y exportarlos a formato graphml


#___________________________________________________________________________________________________
# Función para leer árboles de decisión producidos por Logical Decision
load_tree<-function( file, indexfile, col_id, cold_weight, col_func ) {
  graph<-read.xlsx( file = file, sheetIndex = 1, startRow = 5, endRow = 79,
                    colIndex = c(1,2,5), colClasses = c('character','character' ) )
  graph<-graph[,c(2,3,1)]
  graph<-data.frame( id = paste( 'n', 0:( nrow(graph) - 1), sep = '' ), graph )
  graph<-data.frame( graph, correct_char2( graph, 2 ) )
  colnames(graph)<-c( 'id', 'nom', 'parent', 'type', 'function' )
  
  cols<-c( colnames( indexfile )[ c( col_id, col_weight, col_func ) ] )
  graph<-merge( graph, indexfile[,cols], by.x = 'function', by.y = cols[3], all.x = TRUE  )
  ncl<-ncol(graph)
  graph<-graph[,c( 2, ncl - 1, ncl, 3, 1, 4:(ncl-2) ) ]
  graph<-graph[ order( graph[,1] ), ]
  rownames( graph )<-NULL
  return( graph )
}

load_tree1<-function( file, indexfile, sheet, cols, rows, cols_index ) {
  graph<-read.xlsx( file = file, sheetIndex = sheet, startRow = rows[1], endRow = rows[2],
                    colIndex = cols, colClasses = c('integer','character', 'integer' ) )
  colnames( graph )<-c('id','nom','parent','cod')
  graph<-merge( graph, subset( graph, select = c('id','nom') ),
                by.x = 'parent', by.y = 'id', all.x = TRUE )
  colnames( graph )<-c( 'id_parent', 'id', 'nom', 'cod', 'parent' )
  
  index<-indexfile
  colnames( index )<-c('cod','function','weight')
  graph<-merge( graph, index, by = 'cod', all.x = TRUE  )
  graph<-graph[ ,c( 'id', 'id_parent', 'cod', 'nom', 'parent', 'function', 'weight' ) ]
  graph<-graph[ order( graph$id ), ]
  graph$id<-paste( 'n', graph$id, sep = '' )
  rownames( graph )<-NULL
  return( graph )
}

#___________________________________________________________________________________________________
# Función para escribir estructura de árbol en formato graphml
write_tree<-function( file, tree, col_id, col_cod, col_nom, col_parent, col_weight, col_func, 
                      color_node ) {
  grphxml<-"<?xml version=\"1.0\" encoding=\"UTF-8\"?>
  <graphml xmlns=\"http://graphml.graphdrawing.org/xmlns\"
  xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"
  xsi:schemaLocation=\"http://graphml.graphdrawing.org/xmlns
  http://graphml.graphdrawing.org/xmlns/1.0/graphml.xsd\">

  <key id=\"cod\" for=\"node\" attr.name=\"codigo\" attr.type=\"double\"/>
  <key id=\"c\" for=\"node\" attr.name=\"color\" attr.type=\"string\">
    <default>yellow</default>
  </key>
  <key id=\"n\" for=\"node\" attr.name=\"name\" attr.type=\"string\">
    <default>Nombre</default>
  </key>
  <key id=\"w\" for=\"node\" attr.name=\"weight\" attr.type=\"double\">
    <default>0.0</default>
  </key>
  <key id=\"rw\" for=\"node\" attr.name=\"rweight\" attr.type=\"double\">
    <default>0.0</default>
  </key>
  <key id=\"lf\" for=\"node\" attr.name=\"leaf\" attr.type=\"double\">
    <default>1.0</default>
  </key>
  <key id=\"f\" for=\"node\" attr.name=\"function\" attr.type=\"string\"/>
  <key id=\"d\" for=\"node\" attr.name=\"deep\" attr.type=\"double\"/>

<graph id=\"G\" edgedefault=\"directed\">
"
  nodes<-NULL
  edges<-NULL
  j<-0
  for ( i in 1:nrow(tree) ) { #i<-2
    if ( tree[i,col_nom] != tree[i,col_parent] ) {
      node<-paste( "\t<node id=\"", tree[i,col_id], "\">\n",
                   "\t\t<data key=\"cod\">", ifelse( !is.na( tree[i,col_cod] ), 
                                                     tree[i,col_cod], '' ), "</data>\n",
                    "\t\t<data key=\"n\">", tree[i,col_nom], "</data>\n",
                    "\t\t<data key=\"c\">", color_node, "</data>\n",
                    "\t\t<data key=\"w\">", ifelse( !is.na( tree[i,col_weight] ), 
                                                    tree[i,col_weight], '' ), "</data>\n",
                   "\t\t<data key=\"rw\">", "", "</data>\n",
                    "\t\t<data key=\"lf\">", ifelse( !is.na( tree[i,col_weight] ), 
                                                    1, 0 ), "</data>\n",
                    "\t\t<data key=\"f\">", ifelse( !is.na( tree[i,col_weight] ), 
                                                    tree[i,col_func], ''), "</data>\n",
                   "\t\t<data key=\"d\">", "", "</data>\n",
                    "\t</node>\n", sep = '' )
      edge<-paste( "\t<edge id=\"e", j, "\" ", "directed=\"true\" ", "source=\"", 
                   tree[ tree[,col_nom] == tree[i,col_parent], col_id ],
                   "\" target=\"", tree[i,col_id], "\"/>\n", sep = '' )
      edges<-paste( edges, edge, sep = '' )
      j<-j+1
    } else {
      node<-paste( "\t<node id=\"", tree[i,col_id], "\">\n",
                   "\t\t<data key=\"cod\">", "", "</data>\n",
                 "\t\t<data key=\"n\">", tree[i,col_nom], "</data>\n",
                 "\t\t<data key=\"c\">", color_node, "</data>\n",
                 "\t\t<data key=\"w\">", "", "</data>\n",
                 "\t\t<data key=\"rw\">", "", "</data>\n",
                 "\t\t<data key=\"lf\">", 0, "</data>\n",
                 "\t\t<data key=\"f\">", "", "</data>\n",
                 "\t\t<data key=\"d\">", "", "</data>\n",
                 "\t</node>\n", sep = '' )
    }
    nodes<-paste( nodes, node, sep = '' )
  }
  
  grphxml<-paste( grphxml, nodes, edges, "</graph>\n</graphml>", sep = '' )
  write( grphxml, file = file, append = FALSE ) 
}

save_tree<-function( tree, file ) {
  grphxml<-"<?xml version=\"1.0\" encoding=\"UTF-8\"?>
  <graphml xmlns=\"http://graphml.graphdrawing.org/xmlns\"
  xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"
  xsi:schemaLocation=\"http://graphml.graphdrawing.org/xmlns
  http://graphml.graphdrawing.org/xmlns/1.0/graphml.xsd\">
  
  <key id=\"cod\" for=\"node\" attr.name=\"codigo\" attr.type=\"double\"/>
  <key id=\"c\" for=\"node\" attr.name=\"color\" attr.type=\"string\">
    <default>yellow</default>
  </key>
  <key id=\"n\" for=\"node\" attr.name=\"name\" attr.type=\"string\">
    <default>Nombre</default>
  </key>
  <key id=\"w\" for=\"node\" attr.name=\"weight\" attr.type=\"double\">
    <default>0.0</default>
  </key>
  <key id=\"rw\" for=\"node\" attr.name=\"rweight\" attr.type=\"double\">
    <default>0.0</default>
  </key>
  <key id=\"lf\" for=\"node\" attr.name=\"leaf\" attr.type=\"double\">
    <default>1.0</default>
  </key>
  <key id=\"f\" for=\"node\" attr.name=\"function\" attr.type=\"string\"/>
  <key id=\"d\" for=\"node\" attr.name=\"deep\" attr.type=\"double\"/>

<graph id=\"G\" edgedefault=\"directed\">
"
  nodes<-NULL
  for( i in 1:length( V(tree)$id ) ) {
    node<-paste( "\t<node id=\"", V(tree)$id[i], "\">\n",
                 "\t\t<data key=\"cod\">", V(tree)$codigo[i], "</data>\n",
                 "\t\t<data key=\"n\">", V(tree)$name[i], "</data>\n",
                 "\t\t<data key=\"c\">", V(tree)$color[i], "</data>\n",
                 "\t\t<data key=\"w\">", V(tree)$weight[i], "</data>\n",
                 "\t\t<data key=\"rw\">", V(tree)$rweight[i], "</data>\n",
                 "\t\t<data key=\"lf\">", V(tree)$leaf[i], "</data>\n",
                 "\t\t<data key=\"f\">", V(tree)$'function'[i], "</data>\n",
                 "\t\t<data key=\"d\">", V(tree)$deep[i], "</data>\n",
                 "\t</node>\n", sep = '' )
    nodes<-paste( nodes, node, sep = '' )
  }
  
  edges<-NULL
  for( i in 1:length( E(tree)$id ) ) {
    edge<-paste( "\t<edge id=\"e", i, "\" ", "directed=\"true\" ", 
                 "source=\"", V(tree)[ get.edge(tree,i)[1] ]$id, 
                 "\" target=\"", V(tree)[ get.edge(tree,i)[2] ]$id, "\"/>\n", sep = '' )
    edges<-paste( edges, edge, sep = '' )
  }
  
  grphxml<-paste( grphxml, nodes, edges, "</graph>\n</graphml>", sep = '' )
  write( grphxml, file = file, append = FALSE ) 
  
}

#___________________________________________________________________________________________________
# Función para asignar pesos a las ramas internas dados los pesos en las raíces
sum_weights<-function( tree ) {
  noleaves<-which( V(tree)$leaf == 0 )
  leaves<-which( V(tree)$leaf == 1 )
  for ( i in noleaves ) { # i<-leaves[1]
    childs<-unlist( neighborhood( tree, 100, V(tree)[i], mode = 'out' ) )
    childs<-childs[ childs %in% leaves ]
    V( tree )[i]$weight<-sum( V( tree )[ childs ]$weight )
  }
  return( tree )
}

divide_weights<-function( tree ) {
  noleaves<-which( V(tree)$leaf == 0 )
  leaves<-which( V(tree)$leaf == 1 )
  for ( i in 1:length( V(tree) ) ) { # i<-leaves[9]
    parent<-unlist( neighborhood( tree, 1, V(tree)[i], mode = 'in' ) )
    parent<-parent[ parent != i ]
    childs<-unlist( neighborhood( tree, 100, V(tree)[i], mode = 'out' ) )
    childs<-childs[ childs %in% leaves ]
    if ( length( parent ) == 1 ) {
      pchilds<-unlist( neighborhood( tree, 100, V(tree)[parent], mode = 'out' ) )
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
    parent<-unlist( neighborhood( tree, 1, V(tree)[i], mode = 'in' ) )
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
