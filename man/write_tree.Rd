\name{write_tree}
\alias{write_tree}
\title{
write_tree
}
\description{
Función para escribir la estructura del árbol de decisión del modelo de evaluación en formato "GraphML".
}
\usage{
write_tree( file, tree, col_id, col_nom, col_parent, col_weight, col_func, color_node )
}
\arguments{
  \item{file}{
  Path donde se guardará el árbol en formato \code{graphml}.
}
  \item{tree}{
  Data frame que contiene la información de los nodos y hojas del árbol.
}
  \item{col_id}{
  Columna que contiene la identificación de los nodos y hojas.
}
  \item{col_nom}{
  Columna que contiene el nombre de los nodos y hojas.
}
  \item{col_parent}{
  Columna que contiene el nombre de los padres de los nodos y hojas.
}
  \item{col_weight}{
  Columna que contiene los pesos del árbol.
}
  \item{col_func}{
  Columna que contiene el nombre de las funciones de los nodos y hojas. 
}
  \item{color_node}{
  Color que se asigna al nodo. 
}
}
\details{

}
\value{
La función \code{write_tree} retorna un archivo \code{graphml} que contiene la estructura del árbol, 
con nodos y hojas, del modelo de evaluación.
}
\references{
GraphML Primer:
http://graphml.graphdrawing.org/primer/graphml-primer.html
}
\author{
Julio Andrade, Pedro Guarderas, Daniel Lagos, Andrés López, Paúl Recalde, Edison Salazar.
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
 \code{\link{load_tree}}
}
\examples{
options( stringsAsFactors = FALSE )

# Nombre de indicadores
nomb_indic<-data.frame(cod=1:4, 
                       nom0=c('PROYECTO INSTITUCIONAL', 'SEGUIMIENTO IMPLEMENTADO',
                              'PROYECTOS DE VINCULACION', 'ACTIVIDADES DE EXTENSION Y DIVULGACION'),
                       nom1=c('Proyecto institucional', 'Seguimiento implementado', 
                              'Proyectos de vinculación', 'Actividades de extensión y divulgación'),
                       nomfun=c('proyecto_institucional', 'seguimiento_implementado', 
                                'proyectos_vinculacion', 'actividades_extension_divulgacion'),
                       tip=c('CUALITATIVA', 'CUALITATIVA', 'CUANTITATIVA', 'CUANTITATIVA'))

# Leyendo pesos del modelo
weights<-read_weights( 'example/weights.xls', 1:5 )

nomind<-subset( nomb_indic, select = c(cod,nom1,nomfun,tip) )
colnames(nomind)<-c( "cod", "nombre", "nomfun", "tip" )
weights<-merge( weights, nomind, by.x='nom', by.y='nomfun', all.x = TRUE )

index_file<-data.frame( nomb_indic[,c(1,4)], weight = weights$weight )

# Funcion load_tree
file<-'example/arbol.xls'
col_id<-1
col_weight<-3
col_func<-2

tree_data<-load_tree( file, index_file, col_id, col_weight, col_func )

# Funcion write_tree
col_id<-1
col_cod<-2
col_nom<-4
col_parent<-6
col_weight<-3
col_func<-5
tree<-tree_data
color_node<-'deepskyblue'
file<-'example/tree.graphml'

write_tree( file, tree, col_id, col_cod, col_nom, col_parent, col_weight, col_func, color_node )
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
