#' Graph Embedding with PHATE
#' 
#' 
#' @examples
#' \donttest{
#' ## load the karate club data
#' data(karate, package="T4network")
#' 
#' ## extract the label
#' karate.lab = karate$label
#' 
#' ## gePHATE with different metrizations
#' karate.eff = gePHATE(karate$A, metrize="effective")
#' 
#' ## prepare for igraph plotting tools
#' obj.igraph = igraph::graph_from_adjacency_matrix(karate$A, mode="undirected")
#' 
#' ## visualize
#' opar <- par(no.readonly=TRUE)
#' par(mfrow=c(1,2))
#' plot(obj.igraph, vertex.color=karate.lab, vertex.label=NA,
#'      main="igraph visualization")
#' plot(obj.igraph, vertex.color=karate.lab, vertex.label=NA, 
#'      layout=karate.eff$embed, main="gePHATE-effective")
#' par(opar)
#' }
#' 
#' @concept embedding
#' @export
gePHATE <- function(graph, ndim=2, nnbd=5, alpha=2, ...){
  ## PREPROCESSING
  mynnbd   = max(1, round(nnbd))
  myalpha  = max(as.double(alpha), 1.0)
  myndim   = max(1, round(ndim))

  mymetric = "effective" # let's ignore the shortest-path distance for now.
  if (all(mymetric=="effective")){
    A = aux_networkinput(graph, "gePHATE", req.symmetric = FALSE, req.binary = TRUE)
  } else {
    A = aux_networkinput(graph, "gePHATE", req.symmetric = TRUE, req.binary = TRUE)
  }
  N = base::nrow(A)
  
  ## Extra paramters
  params  = list(...)
  pnames  = names(params)
  
  if ("abstol"%in%pnames){
    myeps = max(.Machine$double.eps, as.double(params$abstol))
  } else {
    myeps = 1e-8
  }
  if ("maxiter"%in%pnames){
    myiter = max(5, round(params$maxiter))
  } else {
    myiter = 100
  }
  
  ## STEP 1. METRIZATION : output should be a 'dist' object.
  if (all(mymetric=="shortest")){
    D = stats::as.dist(maotai::shortestpath(A))
  } else if (all(mymetric=="effective")){
    D = stats::as.dist(base::sqrt(effective_cnn(A)))
  }
  # if (all(mymetric=="latent")){
  #   run.lsm = lvm4net::lsm(A, D=myndim)
  #   run.emb = run.lsm$lsmEZ
  #   rownames(run.emb) = NULL
  #   D = stats::dist(run.emb)
  # }
  # 
  ## STEP 2. PHATE
  fun_PHATE = utils::getFromNamespace("hidden_PHATE", "maotai")
  obj_PHATE = fun_PHATE(D, nbdk=mynnbd, alpha=myalpha)
  distobj   = PHATE2dist(obj_PHATE$P)
  
  ## STEP 3. MMDS
  MMDS_fun = utils::getFromNamespace("hidden_mmds","maotai")
  MMDS_out = MMDS_fun(distobj, ndim=myndim, maxiter=myiter, abstol=myeps)
  
  ## RETURN
  output = list()
  output$embed = MMDS_out
  return(output)
}




# auxiliary function for PHATE --------------------------------------------
#' @keywords internal
#' @noRd
PHATE2dist <- function(P){ # on the sphere, geodesic distance
  # process
  N   = base::nrow(P)
  Psp = array(0,c(N,N))
  myeps = sqrt(.Machine$double.eps)
  for (n in 1:N){
    tgt = as.vector(P[n,])/base::sum(as.vector(P[n,]))
    Psp[n,] = log(tgt+myeps)
  }
  
  # distance
  distobj = array(0,c(N,N))
  for (i in 1:(N-1)){
    tgti = as.vector(Psp[i,])
    for (j in (i+1):N){
      tgtj = as.vector(Psp[j,])
      if (sqrt(sum((tgti-tgtj)^2)) > myeps){
        distobj[i,j] <- distobj[j,i] <- sqrt(sum((tgti-tgtj)^2))
      }
    }
  }
  # for (i in 1:(N-1)){
  #   for (j in (i+1):N){
  #     distobj[i,j] <- distobj[j,i] <- sqrt(sum(as.vector(Psp[i,]-Psp[j,])^2))
  #   }
  # }
  return(stats::as.dist(distobj))
}

