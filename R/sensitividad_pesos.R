vartot<-function( u, w ) {
  s<-sum(w)
  return( s * sum( w * u * u ) - sum( u * w )^2 )
}

vari<-function( u, w ) {
  s<-sum(w)
  return( w * ( s - w ) * u * u )
}

sens.weight.model<-function( utilidades, weights ) {
  n<-ncol( utilidades )
  m<-nrow( utilidades )
  V<-NULL
  W<-NULL
  U<-as.matrix( utilidades[,2:n] )
  U<-U %*% weights
  for ( i in 1:m ) {
    s<-vari( utilidades[i,2:n], weights ) / vartot( utilidades[i,2:n], weights ) 
    u<-( utilidades[i,2:n] * weights ) / U[i]
    V<-rbind( V, data.table( cod = utilidades[i,1], s ) ) 
    W<-rbind( W, data.table( cod = utilidades[i,1], u ) ) 
  }
  setnames( V, 1:n, names( utilidades ) )
  setnames( W, 1:n, names( utilidades ) )
  V<-melt( data = V, id.vars = 'cod' )
  W<-melt( data = W, id.vars = 'cod' )
  setnames( V, 1:3, c( 'cod', 'ind', 'sens' ) )
  setnames( W, 1:3, c( 'cod', 'ind', 'sens' ) )
  V$ind<-as.numeric( gsub( 'eva_', '', V$ind ) )
  W$ind<-as.numeric( gsub( 'eva_', '', W$ind ) )
  V<-V[ with( V, order( ind, cod ) ), ]
  W<-W[ with( W, order( ind, cod ) ), ]
  return( list( S = W, SP = V ) )
}

plot.sens.weight<-function( V, indicadores.nombres, xlab, title, size.text = 8 ) {
  S<-copy( V )
  parplot<-S[ , list( q25 = quantile( sens, probs = 0.25 )[[1]],
                      med = median( sens ),
                      q75 = quantile( sens, probs = 0.75 )[[1]],
                      max = max( sens ) ),
             by = list( ind ) ]
  
  ind.nom<-data.table( indicadores.nombres )
  setnames( ind.nom, 'id', 'ind' )
  ind.nom<-ind.nom[ , list( ind, nom ) ]
  
  parplot<-merge( parplot, ind.nom, by = 'ind' )
  parplot<-parplot[ with( parplot, order( med ) ), ]
  parplot[ , ind := factor( ind, levels = parplot$ind ) ]
  
  S[ , ind := factor( ind, levels = parplot$ind ) ]
  
  MAX<-min( max( parplot$max ), 1 )
  
  ylabs<-paste( formatC( 100 * seq( 0, MAX, length.out = 16 ), digits = 1, 
                         format = 'f', decimal.mark = ',' ), "%", sep = '' )
  
  getPalette<-colorRampPalette( brewer.pal( 9, "YlGn" ) )
  
  sim_plot<-ggplot( S ) + aes( x = ind, y = sens, fill = ind ) +
    geom_boxplot( notch = FALSE, show_guide = FALSE, colour = "#003333",
                  alpha = 0.8, outlier.colour = "#558899", outlier.size = 2 ) +
    xlab( xlab ) +
    ylab( 'Sensitividad' ) +
    coord_flip() +
    scale_x_discrete( labels = paste( parplot$nom, '   ', parplot$ind, sep = '' ), 
                      expand = c(0,0)  ) + 
    scale_y_continuous( breaks = seq( 0, MAX, length.out = 16 ),
                        labels = ylabs,
                        limits = c( 0, MAX ), expand = c(0.01,0) ) +
    scale_fill_manual( values = getPalette( nrow( parplot ) ) ) +
    theme( plot.margin = unit(c(0.6,0.6,0.6,0.6),"cm"),
           panel.background = element_rect(fill = "white", colour = NA), 
           panel.border = element_blank(), 
           #panel.margin = unit( c(0.01,0.01,0.01,0.01),"cm"),
           panel.grid.major.x = element_line( colour = "grey", size = 0.01, linetype = 1 ),
           panel.grid.major.y = element_line( colour = "grey", size = 0.01, linetype = 1 ),
           panel.grid.minor.x = element_blank(),
           panel.grid.minor.y = element_blank(),
           title = element_text( family = 'Times', colour = 'black', size = size.text,  ),
           axis.ticks.length = unit( 0.15, "cm" ),
           axis.ticks.margin = unit( 0.1, "cm" ),
           axis.text.x = element_text( family = 'Times', colour = 'black', size = size.text ), 
           axis.title.x = element_blank(),
           axis.text.y = element_text( family = 'Times', colour = 'black', size = size.text ), 
           axis.title.y = element_blank(),
           legend.position = "none" ) +
    ggtitle( title )
  
  return( sim_plot )
}
