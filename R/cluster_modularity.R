#' Community Detection with Modularity Optimization
#' 
#' Find the communities of a graph by direct optimization of a modularity score. For simplicity, this function 
#' only accepts a symmetric network of \eqn{\lbrace 0, 1 \rbrace } binary edges.
#' 
#' @param graph one of the followings; (1) an \code{igraph} object, (2) a \code{network} object, or (3) an \eqn{(N\times N)} adjacency matrix.
#' 
#' @return a named list containing\describe{
#' \item{cluster}{a length-\eqn{N} vector of class labels.}
#' \item{modularity}{the computed modularity value.}
#' }
#' 
#' @examples 
#' ## load the data
#' data(karate, package="T4network")
#' 
#' ## community detection
#' karate.modular = T4network::modularity(karate$A)
#' karate.label   = karate.modular$cluster
#' karate.true    = karate$label
#' 
#' ## wrap the graph as 'igraph' object
#' karate.igraph = igraph::graph_from_adjacency_matrix(karate$A, mode="undirected")
#' 
#' ## visualize
#' opar <- par(no.readonly=TRUE)
#' par(mfrow=c(1,2))
#' plot(karate.igraph, vertex.label=NA, vertex.color=karate.true,  main="true label")
#' plot(karate.igraph, vertex.label=NA, vertex.color=karate.label, main="estimated label")
#' par(opar)
#' 
#' @references 
#' \insertRef{modularity2004}{T4network}
#' 
#' @concept cluster
#' @export
modularity <- function(graph){
  ## PREPROCESSING
  A.mat = aux_networkinput(graph, "modularity", req.symmetric = TRUE, req.binary = TRUE)
  A.obj = igraph::graph_from_adjacency_matrix(A.mat, mode="undirected")
  
  ## LOUVAIN ALGORITHM
  runalg = igraph::cluster_fast_greedy(A.obj, merges=FALSE, modularity=FALSE)
  
  ## RETURN
  output = list()
  output$cluster    = runalg$membership
  output$modularity = igraph::modularity(A.obj, membership=runalg$membership)
  return(output)
}