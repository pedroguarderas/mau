# Simulation of constrained weights ----------------------------------------------------------------
#' @title Simulation of constrained weights
#' @description Simulation of weight following a Dirichlet distribution with a concentration 
#' parameter and satisfying sum constraints.
#' @param S simulation
#' @param title title for plot
#' @param xlab label x-axis
#' @param ylab label y-axis
#' @param box.col box color
#' @param box.outlier.col box outlier color
#' @param lines.cols spectrum of colors for simulations
#' @param utility.col color for utility value
#' @param utility.point.col color for point utility value
#' @param text.col text color
#' @return ggplot object of simulation
#' @details Details
#' @author Pedro Guarderas
#' @seealso \code{\link{Sim.Const.Weights}} \code{\link{Sim.Weights}}
#' @import ggplot2
#' @export
Plot.Simulation.Weight<-function( S, title = 'Simulations', xlab = 'ID', ylab = 'Utility',
                                  lines.cols = 'blue', box.col = 'gold', box.outlier.col = 'darkred', 
                                  utility.col = 'darkgreen', utility.point.col = 'darkgreen',
                                  text.col = 'black' ) {
  sim<-copy( S )
  
  sim_first<-sim[ , c(1,2), with = FALSE ]
  setnames( sim_first, c( 'id', 's1' ) )
  sim_first[ , E := paste( formatC( 100 * sim_first$s1, digits = 2, format = 'f', decimal.mark = ',' ), "%", sep = '' ) ]
  
  
  ord<-order( apply( sim[ , 2:ncol(sim), with = FALSE ], 1, FUN = median ) )
  
  sim_first[ , id := factor( id, levels = sim$id[ord] ) ]
  
  sim[ , id := factor( id, levels = sim$id[ord] ) ]
  sim<-melt.data.table( data = sim, id = 'id' )
  colnames(sim)<-c( 'id', 's', 'val' )
  
  sim[ , s := as.numeric( s ) ]
  
  N<-max( sim$s )
  gamma<-colors()[ grep( lines.cols, colors() ) ]
  pcolors<-sample( gamma, N, replace = TRUE )
  sim<-merge( sim, data.table( s = 1:N, col = pcolors ), by = 's' )

  sim_plot<-ggplot( sim ) + 
    geom_line( aes( x = id, y = val, group = s, colour = col ), size = 0.1, alpha = 0.4 ) +
    scale_colour_manual( values = pcolors, name = '', labels = ''  ) +
    geom_boxplot( aes( x = id, y = val ), notch = FALSE, show.legend = FALSE, fill = box.col, alpha = 0.5, outlier.colour = box.outlier.col ) +
    geom_line( data = sim_first, aes( x = id, y = s1, group = 'v' ), colour = utility.col, size = 1.5  ) +
    geom_point( data = sim_first, aes( x = id, y = s1 ), colour = utility.point.col, size = 3  ) +
    geom_text( data = sim_first, aes( x = id, y = s1 + 0.05, label = E ), colour = text.col, size = 5  ) +
    ylab( ylab ) +
    xlab( xlab ) +
    theme_minimal() +
    scale_y_continuous( breaks = seq( 0, 1, 0.05 ),
                        labels = format( seq( 0, 1, 0.05 ), digits = 2 ),
                        limits = c( 0, 1 ) ) +
    theme( legend.position = "none" ) +
    ggtitle( title )
  
  return( sim_plot )
}
