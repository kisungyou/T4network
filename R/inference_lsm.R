#' Fit a Latent Space Model for a Binary Network
#' 
#' 
#' @examples 
#' \donttest{
#' ## load the karate club data
#' data(karate, package="T4network")
#' 
#' ## compute 2-dimensional latent space
#' lsm.embed = lsm(karate$A, ndim=2)
#' 
#' ## prepare for plotting with 'igraph' with fixed layout
#' obj.igraph = igraph::graph_from_adjacency_matrix(karate$A, mode="undirected")
#' 
#' ## visualize
#' opar <- par(no.readonly=TRUE)
#' par(mfrow=c(1,2))
#' plot(obj.igraph, vertex.color=karate$label, vertex.label=NA, 
#'      main="igraph visualization")
#' plot(obj.igraph, vertex.color=karate$label, vertex.label=NA,
#'      layout=lsm.embed$latent, main="Latent space embedding")
#' par(opar)
#' }
#' 
#' @concept inference
#' @export
lsm <- function(graph, ndim=2, nstart=5){
  ## PREPROCESSING
  A       = aux_networkinput(graph, "lsm", req.symmetric = TRUE, req.binary = TRUE)
  mydim   = max(1, round(ndim))
  mystart = max(1, round(nstart))
  
  ## FIT
  # obj.network = network::as.network(A, directed=FALSE)
  # obj.fitted  = latentnet::ergmm(obj.network ~ euclidean(d=mydim), tofit="mle")
  
  run.lsm = lvm4net::lsm(A, D=mydim, nstart=mystart)
  
  
  ## RETURN
  output = list()
  output$coefficient = run.lsm$xiT
  output$latent      = run.lsm$lsmEZ
  rownames(output$latent) = NULL
  return(output)
}