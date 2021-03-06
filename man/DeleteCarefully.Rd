% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/DeleteCarefully.R
\name{DeleteCarefully}
\alias{DeleteCarefully}
\title{Delete Carefully
 This is a helper fucntion for the Fixed and Adaptive strategies. It is called by the functions FixedStrategyAttack and AdaptiveStrategyAttack..
It deletes nodes/edges without trying to delete more nodes/edges
than there are in the graph. It is a bit long and fiddly so is broken out for ease of maintenance.
It takes in a graph and outputs a smaller graph}
\usage{
DeleteCarefully(g, Target, DeleteVect, Number, Name)
}
\arguments{
\item{g}{the graph that is going to be modified}

\item{Target}{whether Nodes or Edges are being targeted.}

\item{DeleteVect}{The vector of names to be deleted}

\item{Number}{The number of Nodes/Edges to be deleted}

\item{Name}{the name of the target e.g "Link", or "name"}
}
\description{
Delete Carefully
 This is a helper fucntion for the Fixed and Adaptive strategies. It is called by the functions FixedStrategyAttack and AdaptiveStrategyAttack..
It deletes nodes/edges without trying to delete more nodes/edges
than there are in the graph. It is a bit long and fiddly so is broken out for ease of maintenance.
It takes in a graph and outputs a smaller graph
}
\examples{
DeleteCarefully(g, Target, DeleteVect, Number, Name)

}
\seealso{
@seealso \code{\link{FixedStrategyAttack}}, \code{\link{AdaptiveStrategyAttack}}
}
