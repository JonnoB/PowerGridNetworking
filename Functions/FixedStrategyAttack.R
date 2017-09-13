FixedStrategyAttack <- function(g, DeletionOrder, Target = "Nodes", Number = 1){
  #This function is used for fixed strategy attacks. It takes as an argument the grid in it's current configuration
  #and delets the next available target on the DeletionOrder vector
  #g: network, an Igraph object
  #DeletionOrder: acharacter vector with the target names in order of deletion
  #Target: an optional string the type of Target is either "Nodes" or Edges"
  #Number: The number of the Target to remove
  
  if(Target == "Nodes"){
    
    RemainingVertices <- get.vertex.attribute(g, "name")
    
    DeleteVect <- data_frame(OriginalTarget = DeletionOrder) %>%
      filter( OriginalTarget %in% RemainingVertices) %>%
      .$OriginalTarget
    
    
    if(vcount(g) > Number ){
      deletetarget <- DeleteVect[1:Number]
    } else {
      
      deletetarget <- DeleteVect[1] 
    }
     
    #remove selected node from network
    g2 <- delete.vertices(g, deletetarget)    
      
  } else {
    
    RemainingVertices <- get.edge.attribute(g, "name")
    
    DeleteVect <- data_frame(OriginalTarget = DeletionOrder) %>%
      filter( OriginalTarget %in% RemainingVertices) %>%
      .$OriginalTarget
    
    if(ecount(g) > Number ){
      deletetarget <- DeleteVect[1:Number]
    } else {
      
      deletetarget <- DeleteVect[1] 
    }
  
    #remove selected node from network
    g2 <- delete.edges(g, deletetarget)    
  }
  
  print(paste("Delete", Target, deletetarget))

  
  
  
  return(g2)
  
}