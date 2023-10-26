
# Make bar plots of utilities ----------------------------------------------------------------------
#' @title Bar plot of utilities
#' @description Create ggplot2 bar plots of the utilities at any level of the decision model
#' @param model data.table obtained with \code{\link{compute_model}}
#' @param colors a list of colors for the bars
#' @param deep the deep to navigate the model object a select the utilities
#' @param title title for the bar plot
#' @param xlab label for horizontal axis
#' @param ylab label for vertical axis
#' @return ggplot2 object.
#' @author Pedro Guarderas
#' \email{pedro.felipe.guarderas@@gmail.com}
#' @examples
#' library( mau )
#' vignette( topic = 'Running_MAUT', package = 'mau' ) 
#' @import data.table
#' @importFrom ggplot2 %+% ggplot geom_bar scale_fill_manual geom_boxplot geom_point geom_text 
#' xlab ylab theme_minimal scale_y_continuous theme ggtitle
#' @export
bar_plot <- function( model, deep, colors, title, xlab, ylab ) {
  with( list( model, deep, colors, title, xlab, ylab ), {
    Deep <- deep
    M <- model[ deep == Deep ]
    m <- M[ , list( u = sum( utility ) ), by = cod ]
    m <- m[ order( u ) ]
    M[ , cod := factor( cod, levels = m$cod, ordered = TRUE ) ]
    
    aux <- unique( M[ , list( id, name ) ] )
    setorder( aux, id )
    M[ , name := factor( name, aux$name, ordered = TRUE ) ]

    ylim <- c( 0, 1.025 * max( m$u ) )    
    ybrk <- seq( ylim[1], ylim[2], length.out = 11 )
    ylbl <- formatC( ybrk, digits = 2, format = 'f' )

    bar <- ggplot( data = M ) + 
      geom_bar( aes( x = cod, y = utility, fill = name ), stat = 'identity' ) +
      scale_fill_manual( name = 'Utilities', values = colors ) +
      scale_y_continuous( breaks = ybrk, labels = ylbl, limits = ylim, 
                          expand = expansion( mult = c( 0, 0.025 ) ) ) +
      ggtitle( title ) +
      ylab( xlab ) +
      xlab( ylab ) +
      coord_flip() +
      theme_minimal() +
      theme( panel.spacing = unit( 0.25, "lines" ),
             panel.grid.major.x = element_line( colour = "grey85", linewidth = 0.40, linetype = 3 ),
             panel.grid.major.y = element_line( colour = "grey85", linewidth = 0.40, linetype = 3 ),
             panel.grid.minor.x = element_blank(),
             panel.grid.minor.y = element_blank(),
             legend.position = 'right' )
    
    return( bar )
  })
}

Bar.Plot <- function( model, deep, colors, title, xlab, ylab ) {
  .Deprecated(
    new = 'bar_plot',
    msg = 'The function Bar.Plot will be replaced by the function bar_plot',
    old = 'Bar.Plot' )
  return( bar_plot( model, deep, colors, title, xlab, ylab ) )
}
