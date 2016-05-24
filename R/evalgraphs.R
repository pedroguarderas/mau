#___________________________________________________________________________________________________
# Funciones para graficos

#___________________________________________________________________________________________________
# Spiders

spiders.plot<-function(data, line_type, title, labels, label_size, label_pos, line.col, pos_scale, 
                       title_leg, col_leg, ncol_leg, pos_legx, pos_legy, line_sd, line_width,
                       lty_legend, start){
  N<-length(labels)
  radial.plot( data, rp.type = line_type, 
               main = title, 
               labels = rep('',N), 
               lwd = line_width,
               radial.pos = seq(0,(N-1)*2*pi/N,2*pi/N),
               point.symbols=1, line.col = line.col, start = start, 
               show.grid.labels = pos_scale, 
               grid.left = T, cex = 1.5, cex.axis = 1.2, cex.lab = 1.2, 
               radial.labels = paste(seq(0,100,20), '%', sep = ''), 
               lty = line_sd,
               radial.lim = c(0,1))
  radial.plot.labels( lengths = c(0.0,rep(label_pos,N)), 
                      radial.pos = c(0,seq(0,(N-1)*2*pi/N,2*pi/N)+start),
                      labels = c('',labels),
                      units = 'radians', cex = label_size  )
  legend(pos_legx,pos_legy,title_leg, ncol=ncol_leg, col=col_leg, 
         lty=lty_legend, lwd=3, cex=1.2, pt.cex = 0.7, bty = 'n',
         text.font=2, text.width=0.1)
}

#___________________________________________________________________________________________________
# Formato para funciones de utilidad ggplot2

util.style<-function( size.font, xsize.label, ysize.label, legend.pos, xlab, ylab, cols ) {
  ut<-ggplot() + 
    scale_y_continuous( breaks = ylab,
                        labels = format( ylab, digits=2, justify = "right",
                                         big.mark = ".", decimal.mark = ",") ) +
    scale_x_continuous( breaks = xlab,
                        labels = format( xlab, digits=2, justify = "right",
                                         big.mark = ".", decimal.mark = ",") ) +
    theme_minimal() +
    theme( legend.position = legend.pos, 
           plot.title = element_text(family = 'Times', vjust=0.4, size=size.font),
           axis.title.x = element_text(family = 'Times', vjust=0.4, size=size.font),
           axis.title.y = element_text(family = 'Times', vjust=0.4, size=size.font ),
           axis.text.x = element_text(family = 'Times', vjust=0.4, size=xsize.label),
           axis.text.y = element_text(family = 'Times', vjust=0.4, size=ysize.label),
           legend.title = element_blank(),
           legend.text = element_text(family = 'Times', vjust = 0.0, size = size.font)) +
    
    scale_color_manual( values = cols ) +
    scale_fill_manual( values = cols )
  return( ut )
}


#___________________________________________________________________________________________________
# Posición de IES en funciones utilidad

fun.util.ies<-function(data,funs,cod.funcion,size.font,xsize.label,ysize.label,legend.pos,
                       cols,col.line,size.line,xlab.plot,ylab.plot){

  funciones.limites<-as.data.table( copy( funs ) )
  funciones.limites<-funciones.limites[ ,list( min = min( min ), max = max( max ) ), 
                                     by = list( cod, fun ) ]
  
  FL<-funciones.limites[ cod == cod.funcion, ]
  
  m<-FL$min
  M<-FL$max
  x<-seq( m, M, length.out = 100 )
  y<-sapply( x, FUN = FL$fun )
  aux<-data.frame ( x = x, y = y )

  xlab.pos<-seq( m, M, length.out = 11 )
  ylab.pos<-seq(0,1,0.1)

  p<-util.style( size.font, xsize.label, ysize.label, legend.pos, xlab.pos, ylab.pos, cols ) +
    geom_line( data = aux, aes( x = x, y = y ), colour = col.line, size = size.line ) +
    geom_point( data = data, aes( x = valor, y = utilidad, colour = nom_imp, fill = nom_imp), 
                size=9, shape=21) +
    xlab( xlab.plot ) +
    ylab( ylab.plot )
  
  aux_niv<-subset( funs, fun == FL$fun, select = c( min, max, nivel ) )
  aux_niv<-data.frame( nivel = unique( c( aux_niv$min, aux_niv$max ) ) )
  aux_niv$val<-sapply( aux_niv$nivel, FUN = FL$fun )
  aux_niv<-subset( aux_niv, !( val %in% c(0) ) )
  q<-NULL
  for( j in 1:nrow( aux_niv ) ) { #j<-1
    xj<-aux_niv$nivel[j]
    yj<-aux_niv$val[j]
    q<-paste( q, " + geom_segment( aes( x = ", xj, ", y = ", 0.0, 
              ", xend = ", xj, ", yend = ", yj, " )",
              ", linetype=5, color = 'skyblue', size = 0.5 )",
              " + geom_segment( aes( x = ", 0.0, ", y = ", yj, 
              ", xend = ", xj, ",yend = ", yj, " )",
              ", linetype=5, color = 'skyblue', size = 0.5 )",
              sep = '' )
  }
  q<-paste( 'p<-p', q, sep = '' )
  eval( parse( text = q ) )
  
  return(p)

}

#___________________________________________________________________________________________________
#

