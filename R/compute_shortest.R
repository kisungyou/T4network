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
#' ## compute shortest-path distance
#' dist.spd = shortest(karate$A)
#' 
#' ## visualize
#' opar <- par(no.readonly=TRUE)
#' par(pty="s")
#' image(dist.spd[,34:1], axes=FALSE, main="shortest-path distance matrix")
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