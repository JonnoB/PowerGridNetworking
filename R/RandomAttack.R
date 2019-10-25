#' Generate a random attack order
#'
#' The most simple attack strategy. Targets either nodes or edges.
#' @param g An igraph object representing a power-grid.
#' @param Target The target of the attack. A string either "Nodes" or "Edges".
#' @param Number The number of targets to generate. An integer the default is set to 1.
#' @param Name The attribute name of the target
#' @export
#' @examples
#' RandomAttack(g, Number = V(g))

RandomAttack <-function(g, Target = "Nodes", Number = 1, Name = "name"){
  #The simplest strategy randomly attacks nodes in the network
  #g: a network an igraph object
  #Target: Either nodes or edges
  #Number the total number of nodes/edges to remove

  if(Target == "Nodes"){
    Out <- sample(get.vertex.attribute(g, name = Name), Number)
  } else if (Target =="Edges") {
    Out <- sample(get.edge.attribute(g, name = Name), Number)
  } else {

    stop("Target must be either 'Nodes' or 'Edges'")
  }

  return(Out)
}
