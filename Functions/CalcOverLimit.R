CalcOverLimit <- function(g, SubstationData, EdgeData){
  #This function calculates which edges are over the limit for the current network configuration and demand
  #production profile.
  
  #Create the edge list from the graph object
  ImpedanceCasc <- as_data_frame(g) %>%
    rename(Bus.1 = from, Bus.2 = to) %>%
    group_by(Bus.1, Bus.2) %>% #trying to stop the non-unique identifier problem
    mutate(Link = paste(Bus.1,Bus.2, 1:n(),sep = "-")) %>% #This allows multiple edges 
    #between the same node pair, it is not certain the data is always correct!
    ungroup %>%
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
      
      print(paste("PowerFlow for componant", .x))
      
      ImpedanceMap <- ImpedanceCasc %>% 
        filter(component == SlackRefCasc$component[.x]) %>%
        select(-component) 
      
      ValidNodesMap <- ValidNodesCasc %>%
        filter(component == SlackRefCasc$component[.x])
      
      PowerFlow(ImpedanceMap, ValidNodesMap, SlackRefCasc[.x,])
      
    })
  
  
  #Join up Power flow with line limit data to know which lines are overlimit and will be shut down
  PowerFlowMat2 <- EdgeData %>%
    select( Link, Link.Limit, Link.Type, LineFlow__1 )%>%
    left_join(PowerFlowMat, ., by = "Link") %>%
    mutate(Over.Limit = abs(MW)>Link.Limit)
}