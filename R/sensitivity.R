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
  
  plt_var<-ggplot( data = var ) +
    ylab("Sensitividad") +
    xlab("Indicador") +
    geom_boxplot( aes( index, value ), colour = 'blue4', fill = 'dodgerblue4', alpha = 0.7,
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
  
