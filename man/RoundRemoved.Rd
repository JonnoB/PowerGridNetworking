% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RoundRemoved.R
\name{RoundRemoved}
\alias{RoundRemoved}
\title{Find Deletion order}
\usage{
RoundRemoved(ListofLists, type = "Edge")
}
\arguments{
\item{ListofLists}{The output of AttackTheGrid, a list of lists containing igraphs.}

\item{type}{Whether Edge or node order will be found. The default is Edge, any other value will mean nodes are used.}
}
\description{
Attack round that each edge/node was deleted is  extracted from the network List of lists produced by AttackTheGrid.
Whether the node/edge was the targeted node/edge is also marked. This currently only works for Nodes not Edges
}
\details{
If not all edges/nodes were deleted during AttackTheGrid, these edges will be filled with NA
}
\seealso{
\code{\link[PowerGridNetworking]{attack_the_grid}}
}
