\name{load_tree}
\alias{load_tree}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
load_tree
}
\description{
Función para leer árboles de decisión producidos por Logical Decisions.
}
\usage{
load_tree(file, indexfile, col_id, cold_weight, col_func)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{file}{
  Path donde se encuentra el árbol en formato xlsx.
}
  \item{indexfile}{
  Data frame que contiene los pesos del árbol.
}
  \item{col_id}{
  Columna del data frame \code{indexfile} que contiene el código de los nodos y de las ramas.
}
  \item{col_weight}{
  Columna del data frame \code{indexfile} que contiene los pesos de los nodos y de las ramas.
}
  \item{col_func}{
  Columna del data frame \code{indexfile} que contiene el nombre las funciones de los nodos y de 
  las ramas.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
  La función \code{load_tree} retorna un data frame que contiene la información necesaria para 
  escribir un árbol de decisión en formato "GraphML".
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Julio Andrade, Pedro Guarderas, Daniel Lagos, Andrés López, Paúl Recalde, Edison Salazar.
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
 \code{\link{write_tree}}
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
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
