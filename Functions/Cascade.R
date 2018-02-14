Cascade <- function(NetworkList, Iteration = 0, StopCascade = Inf){
#This Function iterates through the network removing edges until there are no further overpower edges to remove
  #This function takes a weighted edge list, the substation data on demand and generation as well as the current network
  
  #This function uses the Bus order to choose the slack reference should this be changed?
  
  #It returns the a weighted Edge list of the remaining valid edges.
  #g: a weighted graph where the edges are the impedance
  #SubstationData: a dataframe containin Node name, generation and demand.
  #EdgeData: A dataframe containing Bus.1 and Bus.2 as well as Link limit
  #Iteration: the number of iteration number of the cascade, used to keep track of what is going on
  
     g <- NetworkList[[length(NetworkList)]]
    
    Iteration <- Iteration + 1
    #print(paste("Cascade Iteration", Iteration))
    
    g2 <- CalcOverLimit(g)
    
    #Delete Edges that are over the Limit
    
    DeleteEdges <- as_data_frame(g2) %>%
      mutate(index = 1:n(),
        Over.Limit = abs(PowerFlow) > Link.Limit) %>%
      filter(Over.Limit)
    #DeleteEdges <- (1:ecount(g2))[abs(get.edge.attribute(g2, "PowerFlow")) > get.edge.attribute(g2, "Link.Limit")]
    g2 <- delete.edges(g2, DeleteEdges$index)
    
    #Balence grid after over powered lines and edges are removed
    g2 <- BalencedGenDem(g2, "Demand", "Generation")
    
    #checks the initial network and the final network are equal.
    #If they are not equal then a vector of each element of the graph object is returned, showing which 
    #parts are equal and which are not. If the vector is longer than 1 then it they are not equal
    #If the list is only 1 long and is FALSE then it is also FALSE
    edgesequal <- all_equal(get.edgelist(g), get.edgelist(g2))
    
    #Checking there are edges left prevents trying to find a component in the Slackref and throwing an error.
    CascadeContinues <- !isTRUE(edgesequal)
  
    if(CascadeContinues & Iteration != StopCascade){
     #add the new network into the list
      NetworkList <- c(NetworkList, list(g2))
      #update the list with the new lists created in the cascade
     NetworkList <- Cascade(NetworkList, Iteration, StopCascade)
    }
  
    print(paste("Cascade has completed with", Iteration, "iterations"))
    
  return(NetworkList)
  
}