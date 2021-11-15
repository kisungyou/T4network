#' Community Detection with PHATE : Partitional Approach
#' 
#' 
#' @examples 
#' ## load the data
#' data(karate, package="T4network")
#' 
#' ## community detection & given label
#' lab.true = karate$label
#' lab.eff  = gcpPHATE(karate$A, k=2, metrize="effective")$cluster
#' lab.spd  = gcpPHATE(karate$A, k=2, metrize="shortest")$cluster
#' 
#' ## prepare for plotting with 'igraph' with fixed layout
#' obj.igraph = igraph::graph_from_adjacency_matrix(karate$A, mode="undirected")
#' obj.igraph$layout <- igraph::layout_with_kk
#' 
#' ## visualize
#' opar <- par(no.readonly=TRUE)
#' par(mfrow=c(1,3))
#' plot(obj.igraph, vertex.label=NA, vertex.color=lab.true, main="given label")
#' plot(obj.igraph, vertex.label=NA, vertex.color=lab.eff,  main="gcPHATE-effective")
#' plot(obj.igraph, vertex.label=NA, vertex.color=lab.spd,  main="gcPHATE-shortest")
#' par(opar)
#' 
#' @concept cluster
#' @export
gcpPHATE <- function(graph, k=2, metrize=c("effective","shortest"), nnbd=5, alpha=2){
  ## PREPROCESSING
  mynnbd   = max(1, round(nnbd))
  myalpha  = max(as.double(alpha), 1.0)
  mymetric = match.arg(metrize)
  myk      = max(1, round(k))

  if (all(mymetric=="effective")){
    A = aux_networkinput(graph, "gcpPHATE", req.symmetric = FALSE, req.binary = TRUE)
  } else {
    A = aux_networkinput(graph, "gcpPHATE", req.symmetric = TRUE, req.binary = TRUE)
  }
  N = base::nrow(A)
  
  ## STEP 1. METRIZATION : output should be a 'dist' object.
  if (all(mymetric=="shortest")){
    D = stats::as.dist(maotai::shortestpath(A))
  } else if (all(mymetric=="effective")){
    D = stats::as.dist(base::sqrt(effective_cnn(A)))
  }
  
  ## STEP 2. PHATE
  fun_PHATE = utils::getFromNamespace("hidden_PHATE", "maotai")
  obj_PHATE = fun_PHATE(D, nbdk=mynnbd, alpha=myalpha)
  distobj   = PHATE2dist(obj_PHATE$P)
  
  ## STEP 3. K-MEDOIDS
  fun_clust = utils::getFromNamespace("hidden_kmedoids","maotai")
  out_clust = fun_clust(distobj, nclust=myk)
  
  ## RETURN
  output = list()
  output$cluster = as.vector(out_clust$clustering)
  output$medoids = out_clust$id.med
  return(output)
}