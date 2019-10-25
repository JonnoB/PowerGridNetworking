#' delete_carefully
#' 
#' This is a helper fucntion for the Fixed and Adaptive strategies. 
#' It is called by the functions fixed_strategy_attack and adaptive_strategy_attack
#' It deletes nodes/edges without trying to delete more nodes/edges than there are in the graph. 
#' It is a bit long and fiddly so is broken out for ease of maintenance.
#' @param g the graph that is going to be modified
#' @param target whether nodes or Edges are being targeted
#' @param DeleteVect The vector of names to be deleted. This is taken from the deletion order
#' @param number The number of nodes/Edges to be deleted
#' @param node_name the node_name of the target e.g "edge_name", or "name"
#' @export
#' @seealso \code{\link{fixed_strategy_attack}}, \code{\link{adaptive_strategy_attack}}
#' @examples
#' delete_carefully(g, target, DeleteVect, number, node_name)

delete_carefully <- function(g, target, DeleteVect, number, node_name){
  #This is a helper fucntion for the Fixed and Adaptive strategies. It is called by the functions fixed_strategy_attack and adaptive_strategy_attack.. 
  #It deletes nodes/edges without trying to delete more nodes/edges
  #than there are in the graph. It is a bit long and fiddly so is broken out for ease of maintenance.
  #It takes in a graph and outputs a smaller graph
  #g: the graph that is going to be modified
  #target: whether nodes or Edges are being targeted.
  #DeleteVect: The vector of names to be deleted
  #number: The number of nodes/Edges to be deleted
  #node_name: the node_name of the target e.g "edge_name", or "name"
  if(target == "nodes"){
    if(vcount(g) > number ){
      deletetarget <- DeleteVect[1:number]
    }  else if(vcount(g) == 0){
      deletetarget <- NULL
    }else {
      deletetarget <- DeleteVect[1:(vcount(g)-1)]
    }
    #See the edges version below for and explanation of why this code is here
    delete_seq <- which(get.vertex.attribute(g, node_name) %in% deletetarget)
    #remove selected node from network
    g_2 <- delete.vertices(g, delete_seq)
  } else { #if nodes aren't being deleted, then logically edges are being deleted
    if(ecount(g) > number ){
      deletetarget <- DeleteVect[1:number]
    } else if(ecount(g) == 0){
      deletetarget <- NULL
    }else {
      deletetarget <- DeleteVect[1:(ecount(g)-1)]
    }
    #This converts the edge node_name to a numeric value that can be easily removed/
    #It was added in to the code as edges were not being deleted due to a mismatch between what human edge node_name and the machine edge node_name
    #this addition makes deletion more straight forword.
    delete_seq <- which(get.edge.attribute(g, node_name) %in% deletetarget)
    #remove selected node from network
    g_2 <- delete.edges(g, delete_seq)
  }
  message(paste("Delete", target, deletetarget))
  #add a graph attribute that names the deleted node.this makes it possible to track what was deleted and what overloaded
  g_2 <- set.graph.attribute(g_2, "Removed", deletetarget)
  return(g_2)
}
