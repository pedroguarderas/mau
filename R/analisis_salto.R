#___________________________________________________________________________________________________
# Macro para el análisis de salto 

analisis_salto<-defmacro( S, expr ={
  n<-ncol(S)
  m<-nrow(S)
  D<-NULL
  T<-S[order(S[,2]),]
  for ( i in 2:n ) {
    D<-cbind( D, as.numeric( order(order(T[,i])) - order(T[,2]) ) )
  }
  D<-as.data.frame( cbind( T$cod, D ) )
  colnames( D )<-c('cod',paste('X',1:(n-1),sep=''))
  rm(n,m,T,i)
})


plotsaltos<-function( D ) {
  var<-melt( D, id.vars = 'cod' )
  colnames( var )<-c( 'cod', 'index', 'value' )
  # var$index<-gsub( 'X', '', var$index )
  ord<-order( -apply( ( D[,2:ncol(D)]), 1, FUN = median ) )
  idx<-D$cod[ord]
  var$cod<-factor( var$cod, levels = idx )
  ylim<-c(min(var$value),max(var$value))
  plt<-ggplot() +
    ylab("Indicador") +
    xlab("IES") +
    geom_boxplot( aes( cod, value ), data = var, colour = 'darkgreen', fill = 'forestgreen', 
                  alpha = 0.7, show_guide = TRUE, outlier.size = 3, outlier.colour = 'purple', 
                  outlier.shape = 4 ) +
    ggtitle("Análisis de Saltos") +
    theme_minimal() +
    theme( title = element_text( size=20 ),
           axis.text.x = element_text( angle=0, size=12 ), 
           axis.title.x = element_text( vjust=0.4, size=16),
           axis.text.y = element_text( size=12 ),
           axis.title.y = element_text( vjust=0.4, size=16) ) +
    scale_y_continuous( breaks = seq( ylim[1], ylim[2],1.0), limits = ylim )
  
  return( plt )
}