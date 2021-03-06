% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/embedding_embed2011PJM.R
\name{embed2011PJM}
\alias{embed2011PJM}
\title{Directed Graph Embedding by Perrault-Joncas and Meila (2011)}
\usage{
embed2011PJM(graph, ndim = 2)
}
\arguments{
\item{graph}{one of the followings; (1) an \code{igraph} object, (2) a \code{network} object, or (3) an \eqn{(N\times N)} adjacency matrix.}

\item{ndim}{an embedding dimension for a given graph (default: 2).}
}
\value{
a named list containing
\describe{
\item{embed}{an \eqn{(N\times ndim)} matrix of embedded coordinates.}
\item{field}{an \eqn{(N\times ndim)} matrix of directional information on each one.}
}
}
\description{
Find a low-dimensional embedding of the network given an asymmetric/directed graph using 
some information from Laplacian-type operators. For simplicity, this function only accepts a network of \eqn{\lbrace 0, 1 \rbrace } binary edges.
}
\examples{
\donttest{
## create a simple directed ring graph
library(igraph)
mygraph = graph.ring(10, directed=TRUE)

## embed in R^2
solution = embed2011PJM(mygraph)
coords = solution$embed
fields = solution$field

## extract coordinates information
x0 = coords[,1]; y0 = coords[,2]
x1 = x0 + fields[,1]
y1 = y0 + fields[,2]

## visualize the results
opar <- par(no.readonly=TRUE)
par(mfrow=c(1,2), pty="s")
plot(mygraph, main="directed ring graph")
plot(x0,y0, pch=19, main="embedded in R^2",xlim=c(-0.5,0.5),ylim=c(-0.5,0.5))
text(x0+0.05,y0+0.05,labels=1:10)
arrows(x0,y0,x1,y1,length=0.1)
par(opar)
}

}
\references{
\insertRef{perrault-joncas_directed_2011}{T4network}
}
\concept{embedding}
