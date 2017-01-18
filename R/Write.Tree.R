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
                 "source=\"", V(tree)[ ends(tree,i)[1] ]$id, 
                 "\" target=\"", V(tree)[ ends(tree,i)[2] ]$id, "\"/>\n", sep = '' )
    edges<-paste( edges, edge, sep = '' )
  }
  
  grphxml<-paste( grphxml, nodes, edges, "</graph>\n</graphml>", sep = '' )
  write( grphxml, file = file, append = FALSE ) 
  
}