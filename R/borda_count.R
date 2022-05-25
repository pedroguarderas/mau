# Borda count rank aggregation ---------------------------------------------------------------------
#' @title Borda count
#' @description Rank aggreation with the Borda count method
#' @param R matrix with rakings
#' @return Vector with aggregated raking
#' @author Pedro Guarderas
#' \email{pedro.felipe.guarderas@@gmail.com}
#' @examples
#' @export
borda_count <- function( R ) {
  n <- nrow( R )
  b <- rowSums( apply( R, 2, FUN = function( r ) n - order( -r ) ) )
  return( b ) 
}
