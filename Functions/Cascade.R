Cascade <- function(NetworkList, SubstationData, EdgeData, Iteration = 0){
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
  print(paste("Iteration number", Iteration))
  
  #Create the edge list from the graph object
  ImpedanceCasc <- as_data_frame(g) %>%
    rename(Bus.1 = from, Bus.2 = to) %>%
    group_by(Bus.1, Bus.2) %>% #trying to stop the non-unique identifier problem
    mutate(Link = paste(Bus.1,Bus.2, 1:n(),sep = "-")) %>% #This allows multiple edges 
    #between the same node pair, it is not certain the data is always correct!
    ungroup %>%
    mutate(Y = round(Y,7)) %>%
    select(Bus.1, Bus.2, Link, Y) %>%
    arrange(Link)
  
  #checks that only inlcuded nodes are being counted removes nodes that shouldn't be there. 
  #Using the graph vertex name instead of the edge list means that isolated nodes are included as their own component.
  ValidNodesCasc <- SubstationData %>% 
    filter(Bus.Name %in% V(g)$name) 
  
  #matches each node to it's network componan
  componentmatcher <- data_frame(Bus.Name = names(components(g)$membership), 
                                 component = components(g)$membership)
  
  #adds componants to the valid node data frame so that each section can be individually balanced
  ValidNodesCasc <-ValidNodesCasc %>% left_join(componentmatcher, by = "Bus.Name") 
 
  #Identify DeadIslands and balences load
  ValidNodesCasc <-  BalencedGenDem2(g, 
                                     ValidNodesCasc, 
                                     Demand, Generation.B.....Year.Round...Transport.Model., 
                                     "BusTransferB")
  
  #Remove dead islands
  DeadIslandNodes <- ValidNodesCasc$Bus.Name[ValidNodesCasc$DeadIsland]
  
  ImpedanceCasc <- ImpedanceCasc %>% 
    filter(!(Bus.1 %in% DeadIslandNodes) | !(Bus.2 %in% DeadIslandNodes)  )
  
  ValidNodesCasc <- ValidNodesCasc %>%
    filter(!DeadIsland)
  
  #finds the slack reference in each component
  #Only components that are greater than 1 node need to be included as all 1 node components
  #can never exceed there line limit as not lines are used.
  SlackRefCasc <- ValidNodesCasc %>%
    group_by(component) %>%
    arrange(Bus.Order) %>%
    summarise(Bus.Name = first(Bus.Name),
              Nodes = n()) %>%
    filter(Nodes>1)
 
 #Add on the componant information for the nodes in the Impedance edge list, this allows the power flow to be calculated
 #by component
 ImpedanceCasc <- ImpedanceCasc %>% 
   left_join(componentmatcher, by = c("Bus.1"="Bus.Name")) 
 
 #Calculate power flow for each component of the network as seperate networks
 PowerFlowMat <- 1:nrow(SlackRefCasc) %>%
   map_df(~{
     
     print(paste("PowerFlow for componant", .x, "of Iteration", Iteration))
     
     ImpedanceMap <- ImpedanceCasc %>% 
       filter(component == SlackRefCasc$component[.x]) %>%
       select(-component) 
     
     ValidNodesMap <- ValidNodesCasc %>%
       filter(component == SlackRefCasc$component[.x])
     
     PowerFlow(ImpedanceMap, ValidNodesMap, SlackRefCasc[.x,])
       
   })
 

  #Join up Power flow with line limit data to know which lines are overlimit and will be shut down
  PowerFlowMat2 <- EdgeData %>%
    select( Link, Link.Limit, Link.Type, LineFlow__1 )%>%CascadeList
    left_join(PowerFlowMat, ., by = "Link") %>%
    mutate(Over.Limit = abs(MW)>Link.Limit)
  
  #These links are stil safe and can be kept and turn the new Impedance edge matrix back into a graph
  #Using the valid nodes
  g2 <- PowerFlowMat2 %>%
    filter(!Over.Limit) %>% 
    select(Link) %>%
    left_join(ImpedanceCasc, by = "Link") %>%
    select(-Link, -component) %>%
    graph_from_data_frame(., directed=FALSE, vertices=select(ValidNodesCasc, Bus.Name))
  
  #checks the initial network and the final network are equal.
  #If they are not equal then a vector of each element of the graph object is returned, showing which 
  #parts are equal and which are not. If the vector is longer than 1 then it they are not equal
  #If the list is only 1 long and is FALSE then it is also FALSE
  edgesequal <- all_equal(get.edgelist(g), get.edgelist(g2))
  
  CascadeContinues <- !((edgesequal==TRUE)[1] & length(edgesequal)==1)
  
  if(CascadeContinues){
   #add the new network into the list
    NetworkList <- c(NetworkList, list(g2))
    #update the list with the new lists created in the cascade
   NetworkList <- Cascade(NetworkList, SubstationData, EdgeData, Iteration)
  }

  print("Cascade has finished")
  
return(NetworkList)
  
}