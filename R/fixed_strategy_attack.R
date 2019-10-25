#' Flexible Removal method
#'
#' This Removal method pre-calculates the attack order and removes a fixed number of nodes
#'
#' @param g An igraph object representing a power-grid
#' @param DeltionOrder The order in which nodes will be targeted for deltion. This is a string vector
#' @param node_name the variable which holds the edge/node names.
#' @param target The element that will be attacked. A string, the default is nodes. Can also be Edge.
#' @param number The total number of targets to calulcate the order for
#' @export
fixed_strategy_attack <- function(g, deletion_order, target = "nodes", node_name = "name" , number = 1){
  #This function is used for fixed strategy attacks. It takes as an argument a graph g
  #and deletes the next available target on the deletion_order vector, it outputs a graph g_2
  #g: network, an Igraph object
  #deletion_order: acharacter vector with the target names in order of deletion
  #target: an optional string the type of target is either "nodes" or Edges"
  #number: The number of the target to remove
  #create a vector of either nodes or edge names for deletion
  if(target == "nodes"){
    Remaining <- get.vertex.attribute(g, node_name)
  } else {
    Remaining <- get.edge.attribute(g, node_name)
  }
  #Finds which of the targets are still in the network
  DeleteVect <- tibble(OriginalTarget = deletion_order) %>%
    filter( OriginalTarget %in% Remaining) %>%
    .$OriginalTarget
  if(length(DeleteVect)==0){
    g_2 <- g
  } else {
    g_2 <- delete_carefully(g, target, DeleteVect, number, node_name)
  }
  return(g_2)
}
