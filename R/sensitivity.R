#___________________________________________________________________________________________________
# Funciones para el análisis de sensitividad

anvar1<-function( U, S, W, ecod, ewcod, wcod ) {
  Sk<-apply( S[,2:ncol(S)], 1, FUN = var )
  var<-data.frame( cod = S$cod[ ecod ] )
  
  for( j in 1:length( wcod ) ) {#j<-1
    arr<-NULL
    for( i in 1:length( ecod ) ) {#i<-1
      arr<-c( arr, var( W[ wcod[j], ] * U[ ecod[i], ewcod[j] ] ) / Sk[ ecod[i] ] )
    }
    var<-cbind( var, arr )
  }
  rm( i, j, arr )
  colnames( var )<-c( 'cod', paste( 'sen_', wcod, sep = '' ) )
  return( var )
}

plotanvar1<-function( VAR, ylim ) {
  var<-melt( VAR, id.vars = 'cod' )
  colnames( var )<-c( 'cod', 'index', 'value' )
  var$index<-gsub( 'sen_', '', var$index )
  ord<-order( -apply( ( VAR[,2:ncol(VAR)]), 2, FUN = median ) )
  idx<-1:length( ord )
  idx<-idx[ord]
  var$index<-factor( var$index, levels = idx )
  
  plt_var<-ggplot() +
    ylab("Sensitividad") +
    xlab("Indicador") +
    geom_boxplot( aes( index, value ), data = var, colour = 'blue4', fill = 'dodgerblue4', alpha = 0.7,
                  show_guide = TRUE, outlier.size = 3, outlier.colour = 'red4', outlier.shape = 4 ) +
    ggtitle("Análisis de Sensitividad") +
    theme_minimal() +
    theme( title = element_text( size=20 ),
           axis.text.x = element_text( angle=0, size=12 ), 
           axis.title.x = element_text( vjust=0.4, size=16),
           axis.text.y = element_text( size=12 ),
           axis.title.y = element_text( vjust=0.4, size=16) ) +
    scale_y_continuous( breaks = seq(ylim[1],ylim[2],0.05), limits=ylim )

  return( plt_var )
}

plotsim<-function( S, xlab = 'IES' ) {
  N<-ncol( S ) - 1
  sim<-S
  sim_first<-subset( S, select = c( cod, S1 ) )
  sim_first$E<-paste( formatC( 100 * sim_first$S1, digits = 2, format = 'f', decimal.mark = ',' ), "%", sep = '' )
  ord<-order(apply(sim[,2:ncol(sim)],1,FUN=median))
  sim_first$cod<-factor( sim_first$cod, levels = sim$cod[ord] )
  sim$cod<-factor( sim$cod, levels = sim$cod[ord] )
  sim<-melt( data = sim, id = 'cod' )
  colnames(sim)<-c( 'cod', 'sim', 'val' )
  sim$sim<-as.numeric( sim$sim )
  gamma<-colors()[ grep( 'blue', colors() ) ]
  colores<-rep( sample( gamma, N+1, replace = TRUE ), each = 15 )
  
  sim_plot<-ggplot( sim ) + 
    aes( x = cod, y = val ) +
    geom_line( aes( group = sim, colour = sim), size = 0.1 ) +
    scale_fill_manual( values = colores, name='', labels = ''  ) +
    geom_boxplot( notch = FALSE, show.legend = FALSE, fill = 'gold', 
                  alpha = 0.4, outlier.colour = 'darkred' ) +
    geom_point( aes( x = cod, y = S1 ), data = sim_first, colour = 'orange', size = 5  ) +
    geom_text( aes( x = cod, y = S1 + 0.05, label = E ), data = sim_first, colour = 'black', size = 5  ) +
    ylab( 'Valoración' ) +
    xlab( xlab ) +
    theme_minimal() +
    #scale_x_discrete( labels = institutos$imp_nom[ institutos$cod == S$cod ]  ) + 
    scale_y_continuous( breaks = seq( 0, 1, 0.05 ),
                        labels = format( seq( 0, 1, 0.05 ), digits = 2 ),
                        limits = c( 0, 1 ) ) +
    theme( legend.position = "none" ) +
    ggtitle( "Simulación Valoraciones" )

  return( sim_plot )
}
  
