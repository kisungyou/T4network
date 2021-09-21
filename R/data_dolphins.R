#' Load dolphins data
#' 
#' This data consists of social network of 62 dolphins. This small undirected network 
#' is known to consist of two communities of sizes 41 and 21 nodes each.
#' 
#' @usage data(dolphins)
#' 
#' @examples
#' \donttest{
#' # load the data
#' data(dolphins, package="T4network")
#' }
#' 
#' @format a named list containing\describe{
#' \item{igraph}{an \code{'igraph'} object.}
#' \item{name}{a length-\eqn{62} vector of node names for dolphins.}
#' \item{label}{a length-\eqn{62} vector of known community membership.}
#' }
#' 
#' @concept data
"dolphins"


# 
# # processing : https://rpubs.com/soudey/482532
# dolphins <- read.graph(file='https://networkdata.ics.uci.edu/data/dolphins/dolphins.gml', format='gml')
# dolphins_nodes <- get.data.frame(dolphins, what = 'vertices') %>% mutate(Betweenness = betweenness(dolphins), Closeness = closeness(dolphins))
# dolphins_edges <- get.data.frame(dolphins, what = 'edges')
# 
# 
# name = dolphins_nodes$label
# 
# A = as.matrix(igraph::as_adjacency_matrix(dolphins))
# Aobj = igraph::graph_from_adjacency_matrix(A, mode="undirected")
# 
# dolphins = list()
# dolphins$igraph = Aobj
# dolphins$name  = name
# label = rep(0,62)
# id1 = c(8,20,2,40,28,27,26,55,42,58,7,6,10,14,61,33,57,49,32,23,18)
# id2 = setdiff(1:62, id1)
# label[id2] = 1
# label[id1] = 2
# dolphins$label = label
