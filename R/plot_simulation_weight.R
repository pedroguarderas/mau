# Plot decision MAUT model -------------------------------------------------------------------------
#' @title Plot decision MAUT model with weights simulations
#' @description Spider plot for the decision model considering the weights simulated with a Dirichlet 
#' distributions, every simulation is represented with lines, a box plot is included to account the 
#' behavior of every global utility.
#' @param S first element of the simulation list produced by the function 
#' \code{\link{sim_weights}}, \code{\link{sim_const_weights}}.
#' @param title text for the title plot.
#' @param xlab text for x-axis label.
#' @param ylab text for y-axis label.
#' @param box.col color for the boxes.
#' @param box.outlier.col color for the outlier points representing the extreme observations in the
#' boxplot.
#' @param lines.cols the spectrum of colors for the simulation is selected randomly from a base 
#' color.
#' @param utility.col the main utility value is also plotted with this specific color.
#' @param utility.point.col the line of main utilities is plotted with points represented with this
#' color.
#' @param text.col color for the text values plotted for each utility.
#' @return ggplot object with the plot of simulations.
#' @author Pedro Guarderas
#' @seealso \code{\link{sim_const_weights}} \code{\link{sim_weights}}
#' @importFrom ggplot2 %+% ggplot geom_line scale_colour_manual geom_boxplot geom_point geom_text 
#' xlab ylab theme_minimal scale_y_continuous theme ggtitle unit element_line element_blank
#' @importFrom grDevices colorRampPalette colors
#' @export
plot_sim_weight <- function( S, title = 'Simulations', xlab = 'ID', ylab = 'Utility',
                             lines.cols = 'blue', box.col = 'gold', box.outlier.col = 'darkred', 
                             utility.col = 'darkgreen', utility.point.col = 'darkgreen',
                             text.col = 'black' ) {
  with( S, {
    sim <- copy( S )
    
    sim_first <- sim[ , c(1,2), with = FALSE ]
    setnames( sim_first, c( 'id', 's1' ) )
    sim_first[ , E := paste( formatC( 100 * sim_first$s1, digits = 2, format = 'f', decimal.mark = ',' ), "%", sep = '' ) ]
    
    
    ord <- order( apply( sim[ , 2:ncol(sim), with = FALSE ], 1, FUN = median ) )
    
    sim_first[ , id := factor( id, levels = sim$id[ord] ) ]
    
    sim[ , id := factor( id, levels = sim$id[ord] ) ]
    sim <- melt.data.table( data = sim, id.vars = 'id', measure.vars = names( sim )[2:ncol(sim)] )
    colnames(sim) <- c( 'id', 's', 'val' )
    
    sim[ , s := as.numeric( s ) ]
    
    N <- max( sim$s )
    gamma <- colors()[ grep( lines.cols, colors() ) ]
    pcolors <- sample( gamma, N, replace = TRUE )
    sim <- merge( sim, data.table( s = 1:N, col = pcolors ), by = 's' )
    
    ylim <- c( 0, 1 )
    ybrk <- seq( ylim[1], ylim[2], 0.05 )
    ylbl <- formatC( ybrk, digits = 2, format = 'f' )
    
    sim_plot <- ggplot( sim ) + 
      geom_line( aes( x = id, y = val, group = s, colour = col ), size = 0.1, alpha = 0.4 ) +
      scale_colour_manual( values = pcolors, name = '', labels = ''  ) +
      geom_boxplot( aes( x = id, y = val ), notch = FALSE, show.legend = FALSE, fill = box.col, alpha = 0.5, outlier.colour = box.outlier.col ) +
      geom_line( data = sim_first, aes( x = id, y = s1, group = 'v' ), colour = utility.col, size = 1.5  ) +
      geom_point( data = sim_first, aes( x = id, y = s1 ), colour = utility.point.col, size = 3  ) +
      geom_text( data = sim_first, aes( x = id, y = s1 + 0.05, label = E ), colour = text.col, size = 5  ) +
      ggtitle( title ) +
      ylab( ylab ) +
      xlab( xlab ) +
      theme_minimal() +
      scale_y_continuous( breaks = ybrk, labels = ylbl, limits = ylim ) +
      theme( panel.spacing = unit( 0.25, "lines" ),
             panel.grid.major.x = element_line( colour = "grey85", linewidth = 0.40, linetype = 3 ),
             panel.grid.major.y = element_line( colour = "grey85", linewidth = 0.40, linetype = 3 ),
             panel.grid.minor.x = element_blank(),
             panel.grid.minor.y = element_blank(),
             legend.position = 'none' )
      
    return( sim_plot )
  })
}

Plot.Simulation.Weight <- function( S, title = 'Simulations', xlab = 'ID', ylab = 'Utility',
                                    lines.cols = 'blue', box.col = 'gold', box.outlier.col = 'darkred', 
                                    utility.col = 'darkgreen', utility.point.col = 'darkgreen',
                                    text.col = 'black' ) {
  .Deprecated(
    new = 'plot_sim_weight',
    msg = 'The function Plot.Simulation.Weight will be replaced by the function plot_sim_weight',
    old = 'Plot.Simulation.Weight' )
  return( plot_sim_weight( S, title, xlab, ylab, lines.cols, box.col, box.outlier.col, 
                           utility.col, utility.point.col, text.col ) )
}
