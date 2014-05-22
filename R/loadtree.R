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

#___________________________________________________________________________________________________
# Función para escribir estructura de árbol en formato graphml
write_tree<-function( file, tree, col_id, col_nom, col_parent, col_weight, col_func ) {
  grphxml<-"<?xml version=\"1.0\" encoding=\"UTF-8\"?>
  <graphml xmlns=\"http://graphml.graphdrawing.org/xmlns\"
  xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"
  xsi:schemaLocation=\"http://graphml.graphdrawing.org/xmlns
  http://graphml.graphdrawing.org/xmlns/1.0/graphml.xsd\">
  
  <key id=\"c\" for=\"node\" attr.name=\"color\" attr.type=\"string\">
  <default>yellow</default>
  </key>
  <key id=\"n\" for=\"node\" attr.name=\"name\" attr.type=\"string\">
  <default>Nombre</default>
  </key>
  <key id=\"w\" for=\"node\" attr.name=\"weight\" attr.type=\"double\">
  <default>1.0</default>
  </key>
  <key id=\"f\" for=\"node\" attr.name=\"weight\" attr.type=\"string\"/>

<graph id=\"G\" edgedefault=\"undirected\">
"
  nodes<-NULL
  edges<-NULL
  j<-0
  for ( i in 1:nrow(tree) ) { #i<-2
    if ( tree[i,col_nom] != tree[i,col_parent] ) {
      node<-paste( "<node id=\"", tree[i,col_id], "\">
                 <data key=\"n\">", tree[i,col_nom], "</data>
                 <data key=\"c\">", 'dodgerblue3', "</data>
                 <data key=\"w\">", tree[i,col_weight], "</data>
                 <data key=\"f\">", tree[i,col_func], "</data>
                 </node>\n", sep = '' )
      edge<-paste( "<edge id=\"e", j, "\" source=\"", 
                   tree[ tree[,col_nom] == tree[i,col_parent], col_id ],
                   "\" target=\"", tree[i,col_id], "\"/>\n", sep = '' )
      edges<-paste( edges, edge, sep = '' )
      j<-j+1
    } else {
      node<-paste( "<node id=\"", tree[i,col_id], "\">
                 <data key=\"n\">", tree[i,col_nom], "</data>
                 <data key=\"c\">", 'dodgerblue3', "</data>
                 <data key=\"w\">", tree[i,col_weight], "</data>
                 </node>\n", sep = '' )
    }
    nodes<-paste( nodes, node, sep = '' )
  }
  
  grphxml<-paste( grphxml, nodes, edges, "\t</graph>\n</graphml>", sep = '' )
  write( grphxml, file = file, append = FALSE ) 
}
