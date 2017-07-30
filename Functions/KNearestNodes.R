KNearestNodes <- function(graph, NodeName, k){
  #Given a NodeName this function out puts a character vector of node names for nodes that upto k hops away across the network
  #This allows input data to the network to be subsetted and a new sub network created with appropriate inputs
  #graph: the name of the graph that you want to subset
  #NodeName, the name of the node that will be the root of the graph
  #k: the maximum distance from the root
  
  NodeNumber <- which(names(V(graph)) == NodeName)
  BFS1 <- bfs(graph, root = NodeNumber, unreachable = FALSE, rank = TRUE, dist = TRUE)$dist
  
  TakeNodes <-BFS1[BFS1<= k & !is.na(BFS1<= k)]
  
  names(TakeNodes)
}
