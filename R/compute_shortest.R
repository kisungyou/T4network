#' Compute Shortest-Path Distance
#' 
#' \code{shortest} routine is an implementation of Floyd-Warshall algorithm to find 
#' the shortest path between \eqn{N} nodes for a given graph. For simplicity, this function 
#' only accepts a symmetric network of \eqn{\lbrace 0, 1 \rbrace } binary edges.
#' 
#' @param graph one of the followings; (1) an \code{igraph} object, (2) a \code{network} object, or (3) an \eqn{(N\times N)} adjacency matrix.
#' 
#' @return an \eqn{(N\times N)} matrix of pairwise shortest path distances.
#' 
#' @examples 
#' ## load the karate club data
#' data(karate, package="T4network")
#' 
#' ## extract the label
#' karate.lab = karate$label
#' 
#' ## compute shortest-path distance
#' karate.spd = shortest(karate$A)
#' 
#' ## embed using MDS
#' karate.2d  = stats::cmdscale(stats::as.dist(karate.spd))
#' 
#' ## visualize
#' opar <- par(no.readonly=TRUE)
#' plot(karate.2d, col=karate.lab, pch=19, xlab="x", ylab="y", 
#'      main="Shortest-Path Distance Embedding")
#' par(opar)
#' 
#' @concept compute
#' @export
shortest <- function(graph){
  ## PREPROCESSING
  A = aux_networkinput(graph, "shortest", req.symmetric = TRUE, req.binary = TRUE)
  
  ## COMPUTE AND RETURN
  D = maotai::shortestpath(A)
  return(D)
}