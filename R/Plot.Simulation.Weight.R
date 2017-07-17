# Plot decision MAUT model -------------------------------------------------------------------------
#' @title Plot decision MAUT model with weights simulations
#' @description Plot the decision modelo considering the weights simulated with a Dirichlet 
#' distributions, every simulation is plotted with lines and in addition a box plot is included
#' to account the behavior of every global utility.
#' @param S first element of the simulation list produced by the function 
#' \code{\link{Sim.Weights}}, \code{\link{Sim.Const.Weights}}.
#' @param title text for the title plot
#' @param xlab text for x-axis label
#' @param ylab text for y-axis label
#' @param box.col color for the boxes
#' @param box.outlier.col color for the outlier points representing the extreme observations in the
#' boxplot
#' @param lines.cols the spectrum of colors for the simulation is selected randomly from a base 
#' color
#' @param utility.col the main utility value is also plotted with this specific color
#' @param utility.point.col the line of main utilities is plotted with points represented with this
#' color
#' @param text.col color for the text values plotted for each utility
#' @return ggplot object with the plot
#' @author Pedro Guarderas
#' @seealso \code{\link{Sim.Const.Weights}} \code{\link{Sim.Weights}}
#' @importFrom ggplot2 %+% ggplot geom_line scale_colour_manual geom_boxplot geom_point geom_text xlab ylab theme_minimal scale_y_continuous theme ggtitle
#' @importFrom grDevices colorRampPalette colors
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
  sim<-melt.data.table( data = sim, id.vars = 'id', measure.vars = names( sim )[2:ncol(sim)] )
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
