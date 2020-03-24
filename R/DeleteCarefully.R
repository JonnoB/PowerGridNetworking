#' Delete Carefully
#'  This is a helper fucntion for the Fixed and Adaptive strategies. It is called by the functions FixedStrategyAttack and AdaptiveStrategyAttack..
#' It deletes nodes/edges without trying to delete more nodes/edges
#' than there are in the graph. It is a bit long and fiddly so is broken out for ease of maintenance.
#' It takes in a graph and outputs a smaller graph
#' @param g the graph that is going to be modified
#' @param Target whether Nodes or Edges are being targeted.
#' @param DeleteVect The vector of names to be deleted
#' @param Number The number of Nodes/Edges to be deleted
#' @param Name the name of the target e.g "Link", or "name"
#' @export
#' @seealso @seealso \code{\link{FixedStrategyAttack}}, \code{\link{AdaptiveStrategyAttack}}
#' @examples
#'DeleteCarefully(g, Target, DeleteVect, Number, Name)
#'
DeleteCarefully <- function(g, Target, DeleteVect, Number, Name){
  # This is a helper fucntion for the Fixed and Adaptive strategies. It is called by the functions FixedStrategyAttack and AdaptiveStrategyAttack..
  # It deletes nodes/edges without trying to delete more nodes/edges
  # than there are in the graph. It is a bit long and fiddly so is broken out for ease of maintenance.
  # It takes in a graph and outputs a smaller graph
  # g: the graph that is going to be modified
  # Target: whether Nodes or Edges are being targeted.
  # DeleteVect: The vector of names to be deleted
  # Number: The number of Nodes/Edges to be deleted
  # Name: the name of the target e.g "Link", or "name"

  if(Target == "Nodes"){

    if(vcount(g) > Number ){

      deletetarget <- DeleteVect[1:Number]

    }  else if(vcount(g) == 0){
      deletetarget <- NULL

    }else {

      deletetarget <- DeleteVect[1:(vcount(g)-1)]
    }
    #See the edges version below for and explanation of why this code is here
    delete_seq <- which(get.vertex.attribute(g, Name) %in% deletetarget)
    #remove selected node from network
    g2 <- delete.vertices(g, delete_seq)


  } else { #if nodes aren't being deleted, then logically edges are being deleted

    if(ecount(g) > Number ){
      deletetarget <- DeleteVect[1:Number]
    } else if(ecount(g) == 0){

      deletetarget <- NULL

    }else {

      deletetarget <- DeleteVect[1:(ecount(g)-1)]
    }
    
   #This converts the edge name to a numeric value that can be easily removed/
    #It was added in to the code as edges were not being deleted due to a mismatch between what human edge name and the machine edge name
    #this addition makes deletion more straight forword.
   delete_seq <- which(get.edge.attribute(g, Name) %in% deletetarget)
    
    #remove selected node from network
    g2 <- delete.edges(g, delete_seq)
  }

  #message(paste("Delete", Target, deletetarget))

  #add a graph attribute that names the deleted node.this makes it possible to track what was deleted and what overloaded
  #This will be changed in the matrix version
  g2 <- set.graph.attribute(g2, "Removed", deletetarget)

  return(g2)

}
