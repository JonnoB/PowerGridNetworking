#' Generate a random attack order
#'
#' The most simple attack strategy. Targets either nodes or edges.
#' @param g An igraph object representing a power-grid.
#' @param target The target of the attack. A string either "nodes" or "Edges".
#' @param number The number of targets to generate. An integer the default is set to 1.
#' @param node_name The attribute node_name of the target
#' @export
#' @examples
#' random_attack(g, number = V(g))
random_attack <-function(g, target = "nodes", number = 1, node_name = "name"){
  #The simplest strategy randomly attacks nodes in the network
  #g: a network an igraph object
  #target: Either nodes or edges
  #number the total number of nodes/edges to remove
  if(target == "nodes"){
    Out <- sample(get.vertex.attribute(g, node_name = node_name), number)
  } else if (target =="Edges") {
    Out <- sample(get.edge.attribute(g, node_name = node_name), number)
  } else {
    stop("Target must be either 'nodes' or 'Edges'")
  }
  return(Out)
}
