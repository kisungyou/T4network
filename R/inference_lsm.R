#' Fit a Latent Space Model for a Binary Network
#' 
#' 
#' @examples 
#' ## load the karate club data
#' data(karate, package="T4network")
#' 
#' ## extract the label
#' karate.lab = karate$label
#' 
#' ## compute 2-dimensional latent space
#' karate.lsm = lsm(karate$A, ndim=2)
#' 
#' ## visualize
#' opar <- par(no.readonly=TRUE)
#' plot(karate.lsm$latent, col=karate.lab, pch=19, xlab="x", ylab="y", 
#'      main="Latent Space Embedding")
#' par(opar)
#' 
#' @concept inference
#' @export
lsm <- function(graph, ndim=2){
  ## PREPROCESSING
  A     = aux_networkinput(graph, "lsm", req.symmetric = TRUE, req.binary = TRUE)
  mydim = max(1, round(ndim))
  
  ## FIT
  obj.network = network::as.network(A, directed=FALSE)
  obj.fitted  = latentnet::ergmm(obj.network~euclidean(d=mydim), tofit="mle")
  
  ## RETURN
  output = list()
  output$coefficient = obj.fitted$mle$beta
  output$latent      = obj.fitted$mle$Z
  return(output)
}