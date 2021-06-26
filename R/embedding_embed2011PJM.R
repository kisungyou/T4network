#' Directed Graph Embedding by Perrault-Joncas and Meila (2011)
#' 
#' Find a low-dimensional embedding of the network given an asymmetric/directed graph using 
#' some information from Laplacian-type operators. For simplicity, this function only accepts a network of \eqn{\lbrace 0, 1 \rbrace } binary edges.
#' 
#' @param graph one of the followings; (1) an \code{igraph} object, (2) a \code{network} object, or (3) an \eqn{(N\times N)} adjacency matrix.
#' @param ndim an embedding dimension for a given graph (default: 2).
#' 
#' @return a named list containing
#' \describe{
#' \item{embed}{an \eqn{(N\times ndim)} matrix of embedded coordinates.}
#' \item{field}{an \eqn{(N\times ndim)} matrix of directional information on each one.}
#' }
#' 
#' @examples 
#' \donttest{
#' ## create a simple directed ring graph
#' library(igraph)
#' mygraph = graph.ring(10, directed=TRUE)
#' 
#' ## embed in R^2
#' solution = embed2011PJM(mygraph)
#' coords = solution$embed
#' fields = solution$field
#' 
#' ## extract coordinates information
#' x0 = coords[,1]; y0 = coords[,2]
#' x1 = x0 + fields[,1]
#' y1 = y0 + fields[,2]
#' 
#' ## visualize the results
#' opar <- par(no.readonly=TRUE)
#' par(mfrow=c(1,2), pty="s")
#' plot(mygraph, main="directed ring graph")
#' plot(x0,y0, pch=19, main="embedded in R^2",xlim=c(-0.5,0.5),ylim=c(-0.5,0.5))
#' text(x0+0.05,y0+0.05,labels=1:10)
#' arrows(x0,y0,x1,y1,length=0.1)
#' par(opar)
#' }
#' 
#' @references 
#' \insertRef{perrault-joncas_directed_2011}{T4network}
#' 
#' @concept embedding
#' @export
embed2011PJM <- function(graph, ndim=2){
  ## PREPROCESSING
  W     = aux_networkinput(graph, "embed2011PJM", req.symmetric = FALSE, req.binary = TRUE)
  ndim  = max(1, round(ndim))
  nnode = nrow(W) # number of nodes; size of W
  
  # COMPUTATION
  # PART 1 : estimate embedding coordinates
  S = (W+t(W))/2
  Q = diag(rowSums(S)); Qinv = base::solve(Q)
  V = Qinv%*%S%*%Qinv
  Q1 = diag(rowSums(V))
  Hss = base::solve(Q1,V)
  eigHss = base::eigen(Hss)
  
  if (nnode < (ndim+1)){
    warning("* embed.2011PJM : we don't have enough data points for embedding you want.")
  }
  idstart = max(which.min((abs(eigHss$values) < 1)),2) # for spectral clustering, it's important to exclude non-null ones
  if ((ndim+idstart-1)>nnode){
    stop("* embed.2011PJM : invalid eigendecomposition. Try smaller 'ndim'.")
  }
  Phi = eigHss$vectors[,idstart:min((ndim+idstart-1),nnode)]
  Lbd = eigHss$values[idstart:min((idstart+ndim+1),nnode)]
  
  # PART 2 : Density
  vecpi = as.vector(base::eigen(t(Hss))$vectors[,1])
  vecpi = vecpi/sum(vecpi)
  
  # PART 3 : Estimate Vector Field
  PP  = diag(rowSums(W)); PPinv = solve(PP)
  TT  = PPinv%*%W%*%PPinv
  P1  = diag(rowSums(TT))
  Haa = base::solve(P1,TT)
  
  # PART 4 : compute vector field
  # R = (Phi%*%diag(Lbd) - Haa%*%Phi)/2 # arXiv
  R = (Haa-Hss)%*%Phi/2
  
  # REPORT
  output = list()
  if (any(is.complex(Phi))){
    output$embed = base::Re(Phi)
  } else {
    output$embed = Phi
  }
  if (any(is.complex(R))){
    output$field = base::Re(R)
  } else {
    output$field = R  
  }
  return(output)
}