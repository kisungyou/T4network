## AUXILIARY FUNCTIONS
#  (01) aux_networkinput : 'igraph', 'network', or 'adjacency'
#  (02) aux_geigen       : generalized eigenvalue
#  (03) aux_pinv         : pseudo-inverse





# (01) aux_networkinput ---------------------------------------------------
#   - one of three types
#   - allow : symmetric (default: TRUE)
#   - allow : binary    (default: TRUE)
#' @export
aux_networkinput <- function(graph, fname, req.symmetric=TRUE, req.binary=TRUE){
  # type casting
  if (inherits(graph,"igraph")){
    output = as.matrix(igraph::as_adjacency_matrix(graph))
  } else if (inherits(graph, "network")){
    output = as.matrix.network.adjacency(g)
    rownames(output) = NULL
    colnames(output) = NULL
  } else {
    # matrix
    if (!is.matrix(graph)){
      stop(paste0("* ",fname," : input 'graph' should be a matrix."))
    }
    if ((nrow(graph)!=ncol(graph))){
      stop(paste0("* ",fname," : input 'graph' should be a square matrix."))
    }
    output = graph
  }
  
  # 5-2. symmetric
  if (req.symmetric){
    if (!isSymmetric(output)){
      stop(paste0("* ",fname," : input 'graph' should be a symmetric matrix given 'req.symmetric=TRUE'."))
    }
  }
  # 5-3. binary ?
  if (req.binary){
    uvec = base::unique(as.vector(output))
    if (!all.equal(uvec, c(0,1))){
      stop(paste0("* ",fname," : the input 'graph' should be a network of 0-1 binary edges."))
    }
  }
  
  # return
  return(output)
}


# (02) aux_geigen ---------------------------------------------------------
#' @keywords internal
#' @noRd
aux_geigen <- function(A,B,decreasing=TRUE){
  tmpout = geigen::geigen(A,B) # increasing order
  
  gvals  = tmpout$values
  gvecs  = tmpout$vectors
  nnn    = length(gvals)
  
  if (decreasing){
    gvals = gvals[nnn:1]
    gvecs = gvecs[,nnn:1]
  }
  
  return(list(values=gvals,vectors=gvecs))
}



# (03) aux_pinv -----------------------------------------------------------
#' @keywords internal
#' @noRd
aux_pinv <- function(A){
  svdA      = base::svd(A)
  tolerance = (.Machine$double.eps)*max(c(nrow(A),ncol(A)))*as.double(max(svdA$d))
  
  idxcut    = which(svdA$d <= tolerance)
  invDvec   = (1/svdA$d)
  invDvec[idxcut] = 0
  
  output = (svdA$v%*%diag(invDvec)%*%t(svdA$u))
  return(output)
}
