#' Flexible Removal method
#'
#' This Removal method pre-calculates the attack order and removes a fixed number of nodes
#'
#' @param g An igraph object representing a power-grid
#' @param DeltionOrder The order in which nodes will be targeted for deltion. This is a string vector
#' @param Target The element that will be attacked. A string, the default is Nodes. Can also be Edge.
#' @param Number The total number of targets to calulcate the order for
#' @export

FixedStrategyAttack <- function(g, DeletionOrder, Target = "Nodes", Number = 1){
  #This function is used for fixed strategy attacks. It takes as an argument a graph g
  #and deletes the next available target on the DeletionOrder vector, it outputs a graph g2
  #g: network, an Igraph object
  #DeletionOrder: acharacter vector with the target names in order of deletion
  #Target: an optional string the type of Target is either "Nodes" or Edges"
  #Number: The number of the Target to remove

  if(Target == "Nodes"){
    Remaining <- get.vertex.attribute(g, "name")
  } else {

    Remaining <- get.edge.attribute(g, "name")
  }


  DeleteVect <- data_frame(OriginalTarget = DeletionOrder) %>%
    filter( OriginalTarget %in% Remaining) %>%
    .$OriginalTarget


  if(length(DeleteVect)==0){

    g2 <- g

  } else {

    g2 <- DeleteCarefully(g, Target, DeleteVect, Number)

  }



  return(g2)

}
