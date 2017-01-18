vartot<-function( u, w ) {
  s<-sum(w)
  return( s * sum( w * u * u ) - sum( u * w )^2 )
}

vari<-function( u, w ) {
  s<-sum(w)
  return( w * ( s - w ) * u * u )
}

# Sensitivity to weight ----------------------------------------------------------------------------
#' @title Sensitivity to weight
#' @description Create decision tree for MAUT models exporting to igraph struture
#' @param utilities utilities
#' @param weights weights
#' @return var
#' @details Details
#' @author Pedro Guarderas, Andrés Lopez
#' @seealso \code{\link{Eval.Utilities}}
#' @examples
#' Weight.Sensitivity( utilidades, weights )
#' @export
Weight.Sensitivity<-function( utilities, weights ) {
  n<-ncol( utilities )
  m<-nrow( utilities )
  
  URS<-NULL
  
  UG<-as.matrix( utilities[,2:n] )
  UG<-UG %*% weights
  
  ind<-as.numeric( gsub( 'eva_', '', names( utilities[,2:n] ) ) )
  for ( i in 1:m ) {
    s<-as.numeric( vari( utilities[i,2:n], weights ) / vartot( utilities[i,2:n], weights ) )
    u<-as.numeric( ( utilities[i,2:n] * weights ) )
    r<-as.numeric( u / UG[i] )
    
    URS<-rbind( URS, data.table( cod = utilities[i,1], ind = ind, u = u, r = r, s = s ) ) 

  }
  URS<-URS[ with( URS, order( ind, cod ) ), ]

  return( list( URS = URS, UG = UG ) )
}

#---------------------------------------------------------------------------------------------------
plot.sens.weight<-function( V, col, indicadores.nombres, xlab, title, size.text = 8, palette = "YlGn" ) {
  S<-copy( V )
  setnames( S, col, 'sens' )
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
  
  getPalette<-colorRampPalette( brewer.pal( 9, palette ) )
  
  sim_plot<-ggplot( S ) + aes( x = ind, y = sens, fill = ind ) +
    geom_boxplot( notch = FALSE, show.legend = FALSE, colour = "#003333",
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
#            axis.ticks.margin = unit( 0.1, "cm" ),
           axis.text.x = element_text( family = 'Times', colour = 'black', size = size.text ), 
           axis.title.x = element_blank(),
           axis.text.y = element_text( family = 'Times', colour = 'black', size = size.text ), 
           axis.title.y = element_blank(),
           legend.position = "none" ) +
    ggtitle( title )
  
  return( sim_plot )
}
