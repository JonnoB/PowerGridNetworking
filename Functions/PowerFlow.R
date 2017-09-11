PowerFlow <- function(g, SlackRef){
#Calculates the PowerFlow from a graph that contains the following attributes
  #named edges, edgeweights, a balanced power generation and demand column, Powerflow (will be overwritten)
  #g: an igraph object
  #SlackRef: the node to remove from the calculations to prevent the matrix being singular
  
  AZero <- CreateTransmission(g, "Link")
  
  # #remove Slack bus, usually the largest generator
  #drop = FALSE stops the matrix being converted to a vector when there are only two nodes in the sub-graph
  A <- AZero[,colnames(AZero)!=SlackRef, drop = FALSE]
  
  #Create the diagonal matrix of edge to itself impedance
  C <- LinePropertiesMatrix(g)
  
  B <- t(A) %*% C %*% A
  
  InjectionVector <- get.vertex.attribute(g, "BalencedPower")[get.vertex.attribute(g, "name")!=SlackRef]

  Power <- C %*% A %*% solve(B, InjectionVector)
  
    g <- set_edge_attr(g, "PowerFlow", value = Power)

  return(g)
}
