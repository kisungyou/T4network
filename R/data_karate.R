#' Load Zachary's Karate club network
#' 
#' This is the famous Karate club network data, which contains the adjacency matrix 
#' and class label only.
#' 
#' @usage data(karate)
#' 
#' @examples
#' \donttest{
#' # load the data
#' data(karate, package="T4network")
#' }
#'
#' @format a named list containing\describe{
#' \item{A}{a \eqn{(34\times 34)} adjacency matrix that is binary and symmetric.}
#' \item{label}{a length-\eqn{34} vector of class membership.}
#' }
#' 
#' @references 
#' Zachary WW (1977). "An Information Flow Model for Conflict and Fission in Small Groups." \emph{Journal of Anthropological Research}, \bold{33}(4):452-473.
#' 
#' @concept data
"karate"