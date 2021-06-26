#' Directed Graph Embedding by Chen, Yang, and Tang (2007)
#' 
#' Find a low-dimensional embedding of the network given an asymmetric/directed graph. 
#' For simplicity, this function only accepts a network of \eqn{\lbrace 0, 1 \rbrace } binary edges.
#' 
#' @param graph one of the followings; (1) an \code{igraph} object, (2) a \code{network} object, or (3) an \eqn{(N\times N)} adjacency matrix.
#' @param ndim an embedding dimension for a given graph (default: 2).
#' @param alpha perturbation factor \eqn{\in (0,1)} (default: 0.01).
#' 
#' @return a named list containing\describe{
#' \item{embed}{an \eqn{(N\times ndim)} matrix of embedded coordinates.}
#' }
#' 
#' @examples 
#' \donttest{
#' ## create a simple directed ring graph
#' library(igraph)
#' mygraph = graph.ring(10, directed=TRUE)
#' 
#' ## embed in R^2
#' embed2  = embed2007CYT(mygraph)$embed
#' 
#' ## visualize the results
#' opar <- par(no.readonly=TRUE)
#' par(mfrow=c(1,2), pty="s")
#' plot(mygraph, main="directed ring graph")
#' plot(embed2, pch=19, main="2-dimensional embedding",
#'      xlab="dimension 1", ylab="dimension 2")
#' par(opar)
#' } 
#' 
#' @references 
#' \insertRef{chen_directed_2007}{T4network}
#' 
#' @concept embedding
#' @export
embed2007CYT <- function(graph, ndim=2, alpha=0.01){
  ## PREPROCESSING
  W     = aux_networkinput(graph, "embed2007CYT", req.symmetric = FALSE, req.binary = TRUE)
  ndim  = max(1, round(ndim))
  nnode = base::nrow(W)
  if ((alpha <= 0)||(alpha >= 1)){
    stop("* embed2007CYT : perturbation factor 'alpha' should be a small number in (0,1).")
  }
  
  ## PRELIMINARY COMPUTATION
  dout = rowSums(W)       # out-degree
  mu   = rep(0,nnode)     # 1 if i-th row of W is all zeros
  ee   = rep(1,nnode)
  for (i in 1:nnode){
    if (all(as.vector(W[i,])==0)){
      mu[i] = 1
    } 
  }
  
  ## MAIN COMPUTATION
  #   1. teleport random walk
  doutinv = 1/dout
  doutinv[is.infinite(doutinv)] = 0
  P = alpha*(diag(doutinv)%*%W + (1/nnode)*base::outer(mu,ee)) + (1-alpha)*outer(ee,ee)/nnode
  
  #   2. stationary distributions
  vecpi = as.vector(base::Re(base::eigen(t(P))$vectors[,1]))
  vecpi = vecpi/sum(vecpi)
  
  #   3. graph Laplacian
  Phi = diag(vecpi)
  L   = Phi - (Phi%*%P + t(P)%*%Phi)/2
  
  #   4. generalized eigenvalue problem
  geigsol  = aux_geigen(L, Phi, decreasing = FALSE)
  solution = geigsol$vectors[,2:(2+ndim-1)]
  
  ## RETURN
  output = list()
  output$embed = solution
  return(output)
}