# Spider plot --------------------------------------------------------------------------------------
#' @title Spider plot
#' @description Generates an spider plot for a decision model
#' @param data data.table with the utilities of a decision model
#' @param data.label data label
#' @param data.fill data fill color
#' @param data.color data color
#' @param data.linetype line type for data
#' @param data.alpha alpha scale for data
#' @param data.size line size for data
#' @param data.label.color label color for data
#' @param data.label.size label size for data
#' @param group name for the column of groups
#' @param criteria column name for criteria
#' @param valor column name for utilities
#' @param title plot title
#' @param title.color plot title color
#' @param title.size plot title size
#' @param label.size labels size
#' @param label.color labels color
#' @param label.angle labels angle
#' @param label.position labels position
#' @param theta plot rotation angle
#' @param grid grid for plot
#' @param grid.color grid color
#' @param grid.radius.color grid radius color
#' @param grid.linetype grid line type
#' @param grid.size grid line size
#' @param grid.radius.linetype grid radius line type
#' @param grid.radius.size grid radius line size
#' @param axis axis
#' @param axis.label axis label
#' @param axis.color axis color
#' @param axis.size axis size
#' @param axis.linetype axis line type
#' @param axis.angle axis angle
#' @param axis.label.color axis label color
#' @param axis.label.size axis label size
#' @param axis.label.displace axis label displacement
#' @param axis.label.angle axis label angel
#' @param legend.position label position
#' @param legend.size legend size
#' @param legend.text.color legend text color
#' @param plot.margin plot margin
#' @return ggplot2 object with the spider plot
#' @author Pedro Guarderas, Andrés Lopez
#' @examples
#' # Preparing data
#' library( data.table )
#' library( ggplot2 )
#' n<-10
#' m<-7
#' cols<-sample( colors()[ grepl('(red|blue|olive|darkgree)', colors() ) ], m, replace = TRUE )
#' 
#' data<-data.frame( grp = paste( 'A', sort( rep( 1:m, n ) ), sep = '' ),
#'                   cri = factor( rep( paste( 'c', 1:n, sep = '' ), m ), 
#'                                 levels = paste( 'c', 1:n, sep = '' ), ordered = TRUE ),
#'                   val = runif( m * n ) )
#' 
#' data.label<-paste( 'A', 1:m,  ' class', sep = '' )
#' data.fill<-cols
#' data.color<-cols
#' data.linetype<-rep( 'solid', m )
#' data.alpha<-rep( 0.05, m )
#' data.size<-rep( 0.7, m )
#' data.label.color<-'black'
#' data.label.size<-15
#' 
#' # Spider plot parameters
#' title<-'Spider'
#' title.color<-'red3'
#' title.size<-20
#' 
#' label.size<-rep( 8, n )
#' label.color<-rep( 'steelblue4', n )
#' label.angle<-rep( 0, n )
#' label.position<-rep( 1.1, n )
#' 
#' theta<-pi/2
#' 
#' grid<-sort( c( 0.1, 0.25, 0.5, 0.75, 1.0 ) )
#' grid.color<-'grey'
#' grid.radius.color<-'dodgerblue3'
#' grid.linetype<-'dashed'
#' grid.size<-0.5
#' grid.radius.linetype<-'solid'
#' grid.radius.size<-0.5
#' 
#' axis<-grid # Same as grid
#' axis.label<-paste( 100 * axis, '%', sep = '' )
#' axis.color<-'black'
#' axis.size<-0.7
#' axis.linetype<-'solid'
#' axis.angle<-0.4*pi
#' axis.label.color<-'darkgreen'
#' axis.label.size<-5
#' axis.label.displace<- -0.07
#' axis.label.angle<-0
#' 
#' legend.position<-c(0.9, 0.9)
#' legend.size<-0.5
#' legend.text.color<-'black'
#' 
#' plot.margin<-unit( c( 1.0, 1.0, 1.0, 1.0 ),"cm")
#' 
#' 
#' p<-Spider.Plot( data,
#'                 data.label,
#'                 data.fill,
#'                 data.color,
#'                 data.linetype,
#'                 data.alpha,
#'                 data.size,
#'                 data.label.color, 
#'                 data.label.size,
#'                 grp,
#'                 cri,
#'                 val, 
#'                 title,
#'                 title.color,
#'                 title.size,
#'                 label.size, 
#'                 label.color,
#'                 label.angle,
#'                 label.position,
#'                 theta, 
#'                 grid, 
#'                 grid.color, 
#'                 grid.radius.color,
#'                 grid.linetype, 
#'                 grid.size, 
#'                 grid.radius.linetype, 
#'                 grid.radius.size, 
#'                 axis, 
#'                 axis.label,
#'                 axis.color,
#'                 axis.size, 
#'                 axis.linetype, 
#'                 axis.angle, 
#'                 axis.label.color,
#'                 axis.label.size, 
#'                 axis.label.displace,
#'                 axis.label.angle,
#'                 legend.position,
#'                 legend.size,
#'                 legend.text.color,
#'                 plot.margin )
#' 
#' plot(p)
#' @importFrom ggplot2 aes geom_polygon geom_segment geom_text scale_fill_manual ggplot theme element_text element_blank
#' @importFrom ggplot2 scale_color_manual scale_linetype_manual scale_alpha_manual scale_size_manual guide_legend element_rect
#' @import RColorBrewer
#' @export
Spider.Plot<-function( data, # Datos con los valores a plotear, data.frame 
                       data.label,
                       data.fill,
                       data.color,
                       data.linetype,
                       data.alpha,
                       data.size,
                       data.label.color, 
                       data.label.size,
                       group, # Columna de data por la cual se agrupan los datos
                       criteria, # Columna con los criterios de evaluación, debe ser 
                       valor, # Columna con los valores de los polígonos
                       title, # Título para el plot
                       title.color, # Color del título
                       title.size, # Tamaño del título
                       label.size, # Tamaño de etiquetas
                       label.color, # Color de etiquetas
                       label.angle, # Ángulo de etiqueas
                       label.position, # Posición de etiqueas
                       theta, # Giro del plot
                       grid, # Malla para los radios, vector numérico con valores únicos
                       grid.color, # Color del grid
                       grid.radius.color, # Color de los radios del grid
                       grid.linetype, # Tipo de línea del grid
                       grid.size, # Tamaño de la línea
                       grid.radius.linetype, # Tipo de línea de los radios del grid
                       grid.radius.size, # Tamaño de línea de los radios
                       axis, # Eje para ubicarse en el grid
                       axis.label, # Etiquetas del eje
                       axis.color, # Color del eje
                       axis.size, # Tamaño del eje
                       axis.linetype, # Tipo del eje
                       axis.angle, # Ángulo del eje
                       axis.label.color, # Color para labels
                       axis.label.size, # Tamaño del texto del eje
                       axis.label.displace, # Desplazamiento de los labels con respecto al eje
                       axis.label.angle, # Ángulo de labels
                       legend.position,
                       legend.size,
                       legend.text.color,
                       plot.margin # Márgenes del plot
                       ) {
  
  # Hay veces que está función falla numéricamente, hay que mejorarla
  # Falla al volverse singular la matriz que se invierte
  find.radius<-function( r, tl, tk, t ) {
    M<-matrix( c( r * ( cos( tl ) - cos( tk ) ), -cos( t ),
                  r * ( sin( tl ) - sin( tk ) ), -sin( t ) ), 2, 2, byrow = TRUE )
    b<-c( -r * cos( tk ), -r * sin( tk ) )
    return( solve( M, b )[2] )
  }
  
  Grp<-deparse( substitute( group ) )
  Cri<-deparse( substitute( criteria ) )
  Val<-deparse( substitute( valor ) )
  
  n<-nlevels( data[,Cri] )
  m<-length( grid )
  
  N<-n + 1                     
  
  t<-( seq( 0, 2 * pi, length.out = N ) + theta ) %% (2*pi)
  t<-t[-N]
  
  Data<-data
  Data$x<-Data$val * cos( t[ as.numeric( Data[[Cri]] ) ] )
  Data$y<-Data$val * sin( t[ as.numeric( Data[[Cri]] ) ] )
  
  Labels<-data.frame( label = levels( Data[[Cri]] ) )
  Labels$x<-label.position * cos( t )
  Labels$y<-label.position * sin( t )
  
  ald<-axis.label.displace * c( -sin( axis.angle ), cos( axis.angle ) )
  
  k<-max( which( t <= axis.angle  ) )
  l<-(k+1) %% n
  alpha<-sapply( grid, FUN = find.radius, t[l], t[k], axis.angle )
  
  axis.break.position<-data.frame( x = grid[m] * alpha * cos( axis.angle ),
                                   y = grid[m] * alpha * sin( axis.angle ),
                                   xlab = grid[m] * alpha * cos( axis.angle ) + ald[1],
                                   ylab = grid[m] * alpha * sin( axis.angle ) + ald[2] )
  
  axis.break.position<-data.frame( axis.break.position, label = axis.label )
  
  p<-ggplot( data = Data, aes( x = x, y = y ) ) 
  for ( i in 1:m ) {
    X<-data.frame( x = grid[i] * cos( t ), y = grid[i] * sin( t ) )
    p<-p + geom_polygon( data = X, aes( x = x, y = y ),
                         fill = 'white', 
                         colour = grid.color, 
                         linetype = grid.linetype,
                         size = grid.size,
                         alpha = 0.0 )
  }
  
  p<-p + geom_segment( data = X, aes( x = 0, y = 0, xend = x, yend = y ), 
                       colour = grid.radius.color, 
                       linetype = grid.radius.linetype,
                       size = grid.radius.size )  
  
  p<-p + geom_segment( data = X, 
                       aes( x = 0, y = 0, 
                            xend = axis.break.position[m,1], 
                            yend = axis.break.position[m,2] ), 
                       colour = axis.color, 
                       linetype = axis.linetype,
                       size = axis.size  )
  
  p<-p + geom_text( data = axis.break.position, 
                    aes( x = xlab, y = ylab, label = label ), 
                    size = axis.label.size,
                    color = axis.label.color, 
                    angle = axis.label.angle )
#   
  p<-p + geom_polygon( aes( fill = Data[[Grp]],
                            group = Data[[Grp]],
                            colour = Data[[Grp]], 
                            linetype = Data[[Grp]], 
                            alpha = Data[[Grp]],                                                     
                            size = Data[[Grp]] ),
                       show.legend = TRUE )

  p<-p + scale_fill_manual( values = data.fill, 
                            labels = data.label,
      guide = guide_legend( label.theme = element_text( family = 'Times', angle = 0,
                                                        colour = data.label.color, 
                                                        size = data.label.size ) ) ) + 
    scale_color_manual( values = data.color, guide = "none" ) + 
    scale_linetype_manual( values = data.linetype, guide = "none" ) + 
    scale_alpha_manual( values = data.alpha, guide = "none" ) +
    scale_size_manual( values = data.size, guide = "none" )
  
  p<-p + geom_text( data = Labels, 
                    aes( x = x, y = y, label = label ), 
                    size = label.size,
                    color = label.color, 
                    angle = label.angle )
  
#   p<-p + ggtitle( label = title )

  p<-p + theme( plot.margin = plot.margin,
                panel.background = element_rect( fill = "white", colour = NA ), 
                panel.border = element_blank() , 
                panel.grid.major.x = element_blank(),
                panel.grid.major.y = element_blank(),
                panel.grid.minor.x = element_blank(),
                panel.grid.minor.y = element_blank(),
                title = element_text( family = 'Times', colour = title.color, size = title.size ),
                axis.ticks = element_blank(),
                axis.text.x = element_blank(), 
                axis.title.x = element_blank(),
                axis.text.y = element_blank(), 
                axis.title.y = element_blank(),
                legend.position = legend.position,
                legend.background =  element_blank(),
                legend.title = element_blank() )
  
  return( p )

}
