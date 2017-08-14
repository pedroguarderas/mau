
# Make bar plots of utilities ----------------------------------------------------------------------
#' @title Bar plot of utilities
#' @description Create ggplot2 bar plots of the utilities at any level of the decision model
#' @param model data.table obtained with \code{\link{Compute.Model}}
#' @param colors a list of colors for the bars
#' @param deep the deep to navigate the model object a select the utilities
#' @param title title for the bar plot
#' @param xlab label for horizontal axis
#' @param ylab label for vertical axis
#' @return ggplot2 bar plot
#' @author Pedro Guarderas
#' @examples
#' # Loading packages ---------------------------------------------------------------------------------
#' library( mau )
#' library( data.table )
#' library( igraph )
#' library( ggplot2 )
#' 
#' # Table of indexes ---------------------------------------------------------------------------------
#' index<-data.table( cod = paste( 'A', 1:10, sep = '' ),
#'                    i1 = c( 0.34, 1, 1, 1, 1, 0.2, 0.7, 0.5, 0.11, 0.8 ),
#'                    i2 = c( 0.5, 0.5, 1, 0.5, 0.3, 0.1, 0.4, 0.13, 1, 0.74 ),
#'                    i3 = c( 0.5, 1.0, 0.75, 0.25, 0.1, 0.38, 0.57, 0.97, 0.3, 0.76 ),
#'                    i4 = c( 0, 0.26, 0.67, 0.74, 0.84, 0.85, 0.74, 0.65, 0.37, 0.92 ) )
#' 
#' # Loading utilities --------------------------------------------------------------------------------
#' file<-system.file("extdata", "utilities.txt", package = "mau" )
#' script<-'utilities.R'
#' lines<-17
#' skip<-2
#' encoding<-'utf-8'
#' functions<-Read.Utilities( file, script, lines, skip, encoding )
#' source( 'utilities.R' )
#' 
#' # Index positions ----------------------------------------------------------------------------------
#' columns<-c( 2, 3, 4, 5 )
#' 
#' # Function names
#' functions<-sapply( c( 'Project',
#'                       'Self implementation',
#'                       'External and local relations',
#'                       'Scope of capabilities' ),
#'                    FUN = Stand.String )
#' names( functions )<-NULL
#' 
#' # Evaluation of utilities --------------------------------------------------------------------------
#' utilities<-Eval.Utilities( index, columns, functions )
#' 
#' # Tree creation ------------------------------------------------------------------------------------
#' file<-system.file("extdata", "tree.csv", package = "mau" )
#' tree.data<-Read.Tree( file, skip = 0, nrow = 8 )
#' tree<-Make.Decision.Tree( tree.data )
#' 
#' # Compute the decision model -----------------------------------------------------------------------
#' weights<-tree.data[ !is.na( weight ) ]$weight
#' model<-Compute.Model( tree, utilities, weights )
#' 
#' # Bar plots ----------------------------------------------------------------------------------------
#' xlab<-'Utility'
#' ylab<-'Institutions'
#' title<-'Criteria utilities'
#' 
#' colors<-c( 'dodgerblue3', 'orange' )
#' deep<-1
#' bar<-Bar.Plot( model, deep, colors, title, xlab, ylab )
#' plot( bar )
#' 
#' colors<-c( 'dodgerblue4', 'orange', 'gold', 'red3' )
#' deep<-2
#' bar<-Bar.Plot( model, deep, colors, title, xlab, ylab )
#' plot( bar )
#' 
#' @import data.table
#' @importFrom ggplot2 %+% ggplot geom_bar scale_fill_manual geom_boxplot geom_point geom_text 
#' xlab ylab theme_minimal scale_y_continuous theme ggtitle
#' @export
Bar.Plot<-function( model, deep, colors, title, xlab, ylab ) {
  Deep<-deep
  M<-model[ deep == Deep ]
  m<-M[ , list( u = sum( utility ) ), by = cod ]
  m<-m[ order( u ) ]
  M[ , cod := factor( cod, levels = m$cod, ordered = TRUE ) ]
  
  UMax<-max( m$u )
  LMax<-( ( round( UMax * 100, 0 ) %/% 5 ) * 5 ) / 100
  
  if ( UMax > LMax ) LMax<-LMax + 0.05
  
  bar<-ggplot( data = M ) + 
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
}