AttackTheGrid <- function(NetworkList, 
                          AttackStrategy, 
                          referenceGrid = NULL, 
                          MinMaxComp = 0.8, 
                          NodesRemoved = NULL, 
                          StopPoint=Inf,
                          CascadeMode = TRUE){
  #This function attacks the grid using a given attack strategy
  #g: Network as an igraph object
  #AttackStrategy: A function that calculates which node to delete the function is is in "quo" form
  #SubstationData: Dataframe that contains data on the susbtations
  #EdgeData: Data frame of Edge data for the network.
  #referenceGrid: the grid that will be used to test the largest component against if NULL it uses the given network
  #MinMaxComp: The minimum size of the maximum component for the process to continue
  if(is.null(NodesRemoved)){
    NodesRemoved <- 1
  }
  
  #gets the last network in the list
  g <- NetworkList[[length(NetworkList)]]

   g <- g[[length(g)]]
  

  if(is.null(referenceGrid)){
    referenceGrid  <- g
  }
  #Set the environment of the Attack strategy to inside the function so that the correct g is used
  AttackStrategy <- set_env(AttackStrategy, get_env())

  #Remove the desired part of the network.
  gCasc<- AttackStrategy %>% 
    eval_tidy 
  
  #Rebalence network
  #This means that the Cascade calc takes a balanced network which is good.
  gCasc <- BalencedGenDem(gCasc, "Demand", "Generation")

  gCasc <- list(gCasc)
  
  if(CascadeMode){
  #this returns a list of networks each of the cascade
  gCasc <- Cascade(gCasc)
  }
  
  #concatanate the new list with the list of lists
  NetworkList2 <- NetworkList
  NetworkList2[[length(NetworkList2)+1]] <-gCasc
  
  #extract the last network from the just completed cascade
  gCascLast <- gCasc[[length(gCasc)]]
    
  #If the largest componant is larger than the MinMaxComp threshold
  #call the function again and delete a new node.

  FractGC <-max(components(gCascLast)$csize)/vcount(referenceGrid)
  
    
  message(paste("Iteration ",NodesRemoved, " Nodes Remaining", vcount(gCascLast), " GC Fract", round(FractGC,2)))
  
         
    if( !(FractGC < MinMaxComp | length(NetworkList2)==StopPoint) ){
    NetworkList2 <- AttackTheGrid(NetworkList2, AttackStrategy, referenceGrid, MinMaxComp, NodesRemoved+1,StopPoint, CascadeMode)
  }
  
  return(NetworkList2)
  
}