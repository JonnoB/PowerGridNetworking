LinePropertiesMatrix <- function(g, Edgename = "Link", weight = "Y"){
  #This function creates the Line properties matrix from the graph
  #g: Input graph which contains an attribute naming the edges and another that provides the edge weight
  
  Link <- get.edge.attribute(g, Edgename)
  C <- matrix(data = 0, nrow = length(Link), ncol = length(Link))
  diag(C) <- get.edge.attribute(g, weight)
  colnames(C)<- Link
  rownames(C)<- Link
  
  return(C)
}