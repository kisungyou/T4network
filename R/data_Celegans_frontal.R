#' Load C. elegans data : neuronal network of frontal neurons
#' 
#' It is a directed neuronal network of frontal neurons consisting of 131 nodes 
#' and 764 binary edges. Additionally, two-dimensional coordinates for all nodes 
#' are given.
#' 
#' @usage data(Celegans.frontal)
#' 
#' @examples
#' \donttest{
#' # load the data
#' data(Celegans.frontal, package="T4network")
#' }
#' 
#' @format a named list containing\describe{
#' \item{igraph}{an \code{'igraph'} object.}
#' \item{name}{a length-\eqn{131} vector of node names.}
#' \item{position}{an \eqn{(131\times 2)} matrix of physical positions.}
#' }
#' 
#' @references 
#' Kaiser M and Hilgetag CC (2006). "Nonoptimal component placement, but short processing paths, due to long-distance projections in neural systems." \emph{PLoS Comput. Biol.}, 2.7:e95.
#' 
#' @concept data
"Celegans.frontal"




# plot(Celegans.frontal$igraph, vertex.label=NA)
# 
# ## load the karate club data
# data(Celegans.frontal, package="T4network")
# 
# ## gePHATE
# Cef.eff = gePHATE(Celegans.frontal$igraph)
# 
# ## visualize
# opar <- par(no.readonly=TRUE)
# par(mfrow=c(1,2))
# plot(obj.igraph, vertex.color=karate.lab, vertex.label=NA,
#      main="igraph visualization")
# plot(obj.igraph, vertex.color=karate.lab, vertex.label=NA,
#      layout=karate.eff$embed, main="gePHATE-effective")
# par(opar)