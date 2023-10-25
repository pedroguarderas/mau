
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
    
    UMax <- max( m$u )
    LMax <- ( ( round( UMax * 100, 0 ) %/% 5 ) * 5 ) / 100
    
    if ( UMax > LMax ) LMax <- LMax + 0.05
    
    bar <- ggplot( data = M ) + 
      geom_bar( aes( x = cod, y = utility, fill = as.character( id ) ), stat = 'identity' ) +
      scale_fill_manual( values = colors ) +
      coord_flip() +
      ylab( ylab ) +
      xlab( xlab ) +
      theme_minimal() +
      scale_y_continuous( breaks = seq( 0, LMax, 0.05 ),
                          labels = format( seq( 0, LMax, 0.05 ), digits = 2 ),
                          limits = c( 0, LMax ) ) +
      theme( legend.position = "none" ) +
      ggtitle( title )
    
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
