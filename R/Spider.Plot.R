
# Spider plot --------------------------------------------------------------------------------------
#' @title Spider plot
#' @description Spider plot
#' @param data Datos con los valores a plotear, data.frame 
#' @param data.label label
#' @param data.fill fill
#' @param data.color color
#' @param data.linetype linetype
#' @param data.alpha alpha
#' @param data.size size
#' @param data.label.color label.color
#' @param data.label.size label.size
#' @param group Columna de data por la cual se agrupan los datos
#' @param criteria Columna con los criterios de evaluación, debe ser 
#' @param valor Columna con los valores de los polígonos
#' @param title Título para el plot
#' @param title.color Color del título
#' @param title.size Tamaño del título
#' @param label.size Tamaño de etiquetas
#' @param label.color Color de etiquetas
#' @param label.angle Ángulo de etiqueas
#' @param label.position Posición de etiqueas
#' @param theta Giro del plot
#' @param grid Malla para los radios, vector numérico con valores únicos
#' @param grid.color Color del grid
#' @param grid.radius.color Color de los radios del grid
#' @param grid.linetype Tipo de línea del grid
#' @param grid.size Tamaño de la línea
#' @param grid.radius.linetype Tipo de línea de los radios del grid
#' @param grid.radius.size Tamaño de línea de los radios
#' @param axis Eje para ubicarse en el grid
#' @param axis.label Etiquetas del eje
#' @param axis.color Color del eje
#' @param axis.size Tamaño del eje
#' @param axis.linetype Tipo del eje
#' @param axis.angle Ángulo del eje
#' @param axis.label.color Color para labels
#' @param axis.label.size Tamaño del texto del eje
#' @param axis.label.displace Desplazamiento de los labels con respecto al eje
#' @param axis.label.angle Ángulo de labels
#' @param legend.position position for the legend
#' @param legend.size legend size
#' @param legend.text.color legend text color
#' @param plot.margin Márgenes del plot
#' @return data.table with utilities
#' @details Details
#' @author Pedro Guarderas, Andrés Lopez
#' @seealso \code{\link{Eval.Utilities}}
#' @import ggplot2
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
