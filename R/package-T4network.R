#' package documentation
#'
#' @noRd
#' @docType package
#' @name T4network
#' @aliases package-T4network
#' @noRd
#' @import Rdpack
#' @import geigen 
#' @importFrom maotai lyapunov shortestpath
#' @importFrom latentnet ergmm
#' @importFrom network as.matrix.network.adjacency
#' @importFrom igraph as_adjacency_matrix graph_from_adjacency_matrix components simplify graph.compose graph.ring graph.star edge is_connected is_directed cluster_fast_greedy modularity plot
#' @importFrom utils packageVersion
#' @importFrom stats as.dist cmdscale
#' @importFrom Rcpp evalCpp
#' @useDynLib T4network, .registration=TRUE
NULL