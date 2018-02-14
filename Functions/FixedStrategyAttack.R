FixedStrategyAttack <- function(g, DeletionOrder, Target = "Nodes", Number = 1){
  #This function is used for fixed strategy attacks. It takes as an argument the grid in it's current configuration
  #and delets the next available target on the DeletionOrder vector
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
  
  
  g2 <- DeleteCarefully(g, Target, DeleteVect, Number)

  return(g2)
  
}