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
  for ( i in 1:m ) {
    s<-vari( utilidades[i,2:n], weights ) / vartot( utilidades[i,2:n], weights ) 
    V<-rbind( V, data.table( cod = utilidades[i,1], s ) ) 
  }
  setnames( V, 1:n, names( utilidades ) )
  V<-melt( data = V, id.vars = 'cod' )
  setnames( V, 1:3, c( 'cod', 'ind', 'sens' ) )
  V[ , ind := as.numeric( gsub( 'eva_', '', ind ) ) ]
  V<-V[ with( V, order( ind, cod ) ), ]
  return( V )
}

plot.sens.weight<-function( V, indicadores.nombres, xlab = 'IES' ) {
  S<-copy( V )
  parplot<-S[ , list( q25 = quantile( sens, probs = 0.25 )[[1]],
                      q50 = quantile( sens, probs = 0.50 )[[1]],
                      q75 = quantile( sens, probs = 0.75 )[[1]],
                      max = max( sens ) ),
             by = list( ind ) ]
  
  ind.nom<-data.table( indicadores.nombres )
  setnames( ind.nom, 'id', 'ind' )
  ind.nom<-ind.nom[ , list( ind, nom ) ]
  
  parplot<-merge( parplot, ind.nom, by = 'ind' )
  parplot<-parplot[ with( parplot, order( q50 ) ), ]
  parplot[ , ind := factor( ind, levels = parplot$ind ) ]
  
  S[ , ind := factor( ind, levels = parplot$ind ) ]
  
  MAX<-round( max( parplot$max ), 2 )
  
  ylabs<-paste( formatC( 100 * seq( 0, 1, length.out = 21 ), digits = 1, 
                         format = 'f', decimal.mark = ',' ), 
                "%", sep = '' )
    
  sim_plot<-ggplot( S ) + 
    aes( x = ind, y = sens ) +
    geom_boxplot( notch = FALSE, show_guide = FALSE, fill = 'gold', 
                  alpha = 0.4, outlier.colour = 'darkred' ) +
    xlab( 'Indicadores' ) +
    ylab( 'Sensitividad' ) +
    coord_flip() +
    theme_minimal() +
    scale_x_discrete( labels = parplot$nom  ) + 
    scale_y_continuous( breaks = seq( 0, 1, length.out = 21 ),
                        labels = ylabs,
                        limits = c( 0, 1 ) ) +
    theme( legend.position = "none" ) +
    ggtitle( "Sensitividad debida a los pesos" )
  
  return( sim_plot )
}
