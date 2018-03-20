# mau ----------------------------------------------------------------------------------------------
#' @title mau
#' @description Provides functions for the creation, evaluation and test of decision models based in
#' Multi Attribute Utility Theory (MAUT).
#'  
#' @details MAUT models are defined employing a decision tree where similarity relations between 
#' different index utilities are defined, this helps to group utilities following a criteria of 
#' similarity. Each final node has an utility and weight associated, the utility of any internal 
#' node in the decision tree is computed by adding the weighted sum of eaf of its final nodes. In a 
#' model with \eqn{n} indexes, a criteria is composed by \eqn{C \subset \{1,\ldots,n\}}, the 
#' respective utility is given by:
#' 
#' \deqn{ \sum_{i \in C}^n w_i u_i( x_i ) }
#' 
#' Currently, each utility is defined like a piecewise risk aversion utility, those functions are 
#' of the following form:
#' \deqn{a x + b}
#' or
#' \deqn{a e^{cx} + b}
#'   
#' The current capabilities of \pkg{mau} are:
#' \enumerate{
#'   \item Read a list of risk aversion utilities defined in a standardized format.
#'   \item Evaluate utilities of a table of indexes.
#'   \item Load decision trees defined in column standard format.
#'   \item Compute criteria utilities and weights for any internal node of the decision tree.
#'   \item Simulate weights employing Dirichlet distributions under addition constraints in weights. 
#' }
#' @examples
#' library( mau )
#' vignette( topic = 'Running_MAUT', package = 'mau' ) 
#' 
#' @importFrom Rdpack reprompt
#' 
#' @references
#' \insertRef{DecMak}{mau}
#' 
#' \insertRef{HarDec}{mau}
#' 
#' \insertRef{UtiThe}{mau}
#' 
#' \insertRef{DecQua:1996}{mau}
#' 
#' \insertRef{DecRis:1992}{mau}
"_PACKAGE"
