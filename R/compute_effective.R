#' Compute Effective Resistance
#' 
#' This function computes the effective resistance, a concept of dissimilarity for nodes. 
#' A notable characteristic is that the square root of effective resistance values 
#' for a given network induces a metric structure of a graph. For simplicity, this function 
#' only accepts a network of \eqn{\lbrace 0, 1 \rbrace } binary edges.
#' 
#' @param graph one of the followings; (1) an \code{igraph} object, (2) a \code{network} object, or (3) an \eqn{(N\times N)} adjacency matrix.
#' 
#' @return an \eqn{(N\times N)} matrix of pairwise effective resistance values.
#' 
#' @examples 
#' ## load the karate club data
#' data(karate, package="T4network")
#' 
#' ## compute effective resistance & compute metrics
#' effres = effective(karate$A)
#' effmet = sqrt(effres)
#' 
#' ## visualize
#' opar <- par(no.readonly=TRUE)
#' par(pty="s")
#' image(effmet[,34:1], axes=FALSE, main="metric matrix from ER")
#' par(opar)
#' 
#' @references 
#' \insertRef{young_new_2016}{T4network}
#' 
#' \insertRef{young_new_2016-1}{T4network}
#' 
#' @concept compute
#' @export
effective <- function(graph){
  ## PREPROCESSING
  A = aux_networkinput(graph, "effective", req.symmetric = FALSE, req.binary = TRUE)
  N = base::nrow(A)
  
  ## MAIN COMPUTATION
  gg = effective_cnn(A)
  if (any(is.na(gg))||any(is.infinite(gg))){
    warning("* effective : input 'graph' may not be connected. 'Inf' values indicate edges between isolated components.")
  }
  
  return(gg)
}




# auxiliary ---------------------------------------------------------------
#' @keywords internal
#' @noRd
effective_cnn <- function(A){ # adjacency matrix
  # parameters and pre-compute L
  if (inherits(A, "dgCMatrix")){
    A = as.matrix(A)
  }
  N = nrow(A)
  diag(A) = rep(0, as.integer(N))
  L = base::diag(base::rowSums(A)) - A # graph laplacian
  
  # 1. compute Q
  tmp = diag(N)
  tmp[,1] = rep(1,N)/sqrt(N)
  qrq = base::qr.Q(base::qr(tmp))
  Q = t(qrq[,2:N])
  
  # 2. compute \bar{L}
  Lbar = Q%*%L%*%t(Q)
  
  # 3. solve Lyapunov equation
  Sigma = maotai::lyapunov(Lbar, diag(rep(1,N-1)))
  
  # 4. compute X
  X = 2*(t(Q)%*%Sigma%*%Q)
  
  # 5. compute 
  out.cpp = cpp_effective(X)
  if (!isSymmetric(out.cpp)){
    out.cpp = (out.cpp + t(out.cpp))/2
  }
  return(out.cpp)
}
#' @keywords internal
#' @noRd
effectivesym <- function(A){
  n = nrow(A)
  L = diag(rowSums(A))-A
  Linv = aux_pinv(L)
  
  output = array(0,c(n,n))
  for (i in 1:(n-1)){
    for (j in (i+1):n){
      est = rep(0,n)
      est[i] = 1
      est[j] = -1
      
      output[i,j] <- output[j,i] <-sum(as.vector(Linv%*%est)*est)
    }
  }
  return(output)
}