# mau ----------------------------------------------------------------------------------------------
#' @title mau
#' @description
#' Build and test decision models employing Multi Attribute Utility Theory (MAUT). Automatic 
#' evaluation of utilities at any level of the decision tree, weight simulations for sensitivity 
#' analysis.
#'  
#' MAUT models are based in a decision tree where similarity relations between different index 
#' utilities are defined, this helps to group utilities following a criteria of similarity. 
#' The global utility can be computed as a linear combination of given weights and the utility of 
#' each index for any other level of the decision tree the utility can be computed by local linear 
#' combitation of weights and child index utilities.
#'  
#' @details  Risk aversion utilites, are linear functions of the form:
#'  \deqn{ a x + b}
#' or exponential form 
#'  
#' \deqn{ a e^{cx} + b}
#' 
#' \deqn{ \sum_{i=1}^n w_i u_i( x_i ) }
#'   
#' The current capabilities of the mau are:
#' \enumerate{
#'   \item Read list of risk aversion utilities defined in an standardized format.
#'   \item Evaluate utilities of a table of indexes.
#'   \item Load decision trees defined in column standard.
#'   \item Compute internal criteria utilities for a decision tree.
#'   \item Simulate weights, under criteria constraints.
#' } 
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
