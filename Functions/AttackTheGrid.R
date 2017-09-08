AttackTheGrid <- function(g, AttackStrategy, SubstationData, EdgeData, referenceGrid = NULL, MinMaxComp = 0.8){
  #This function attacks the grid using a given attack strategy
  #g: Network as an igraph object
  #AttackStrategy: A function that calculates which node to delete the function is is in "quo" form
  #SubstationData: Dataframe that contains data on the susbtations
  #EdgeData: Data frame of Edge data for the network.
  #referenceGrid: the grid that will be used to test the largest component against if NULL it uses the given network
  #MinMaxComp: The minimum size of the maximum component for the process to continue
  
  if(is.null(referenceGrid)){
    referenceGrid  <- g
  }
  
  deletevertex <- eval_tidy(AttackStrategy)
  
  #remove selected node from network
  gCasc <- delete.vertices(g, deletevertex)
  
  gCasc <- Cascade(gCasc, SubstationData, EdgeData)
  
  #If the largest componant is larger than the MinMaxComp threshold
  #call the function again and delete a new node.
  if((max(components(gCasc)$csize)/vcount(referenceGrid)) > MinMaxComp){
    gCasc <- AttackTheGrid(gCasc, AttackStrategy, SubstationData, EdgeData, referenceGrid)
  }
  
  return(gCasc)
  
}