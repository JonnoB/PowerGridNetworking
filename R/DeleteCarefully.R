DeleteCarefully <- function(g, Target, DeleteVect, Number){
  #This is a helper fucntion for the Fixed and Adaptive strategies. It deletes nodes/edges without trying to delete more nodes/edges
  #than there are in the graph. It is a bit long and fiddly so is broken out for ease of maintenance.
  #It takes in a graph and out puts a smaller graph
  #g: the graph that is going to be modified
  #Target: whether Nodes or Edges are being targeted.
  #DeleteVect: The vector of names to be deleted
  #Number: The number of Nodes/Edges to be deleted

  if(Target == "Nodes"){

    if(vcount(g) > Number ){

      deletetarget <- DeleteVect[1:Number]

    }  else if(vcount(g) == 0){
      deletetarget <- NULL

    }else {

      deletetarget <- DeleteVect[1:(vcount(g)-1)]
    }

    #remove selected node from network
    g2 <- delete.vertices(g, deletetarget) #causing the crash... why?


  } else { #if nodes aren't being deleted, then logically edges are being deleted

    if(ecount(g) > Number ){
      deletetarget <- DeleteVect[1:Number]
    } else if(ecount(g) == 0){

      deletetarget <- NULL

    }else {

      deletetarget <- DeleteVect[1:(ecount(g)-1)]
    }

    #remove selected node from network
    g2 <- delete.edges(g, deletetarget)
  }

  message(paste("Delete", Target, deletetarget))

  #add a graph attribute that names the deleted node.this makes it possible to track what was deleted and what overloaded
 g2 <- set.graph.attribute(g2, "Removed", deletetarget)

  return(g2)

}
