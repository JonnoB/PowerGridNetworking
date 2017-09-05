BalencedGenDem2 <- function(g, ValidNodes){
  #Balences the generation and demand across multiple isolated componants
  #g: the graph of the network that is being attacked
  #ValidNodes: the data frame of valid nodes and there demand and generation
  
  componentmatcher <- data_frame(Bus.Name = names(components(g)$membership), 
                                 component = components(g)$membership)
  
  ValidNodesCasc <- ValidNodes %>%
    left_join(componentmatcher, by = "Bus.Name") %>%
    group_by(component) %>%
    #remove Dead islands
    mutate(DeadIsland = sum(Demand)==0 | sum(Generation.B.....Year.Round...Transport.Model.)==0,
           Demand2 = ifelse(DeadIsland, 0, Demand),
           Generation.B2 = if_else(DeadIsland, 0, Generation.B.....Year.Round...Transport.Model.)) %>%
    #rebalence remaining componants
    mutate(GenBal = sum(Demand2)>sum(Generation.B2),
           Demand2 = ifelse(GenBal, 
                            Demand2*(sum(Generation.B2)/sum(Demand2)), Demand2),
           Generation.B2 = ifelse(GenBal, 
                                  Generation.B2*(sum(Demand2)/sum(Generation.B2)), Generation.B2),
           
           BusTransferB = Demand2 + Generation.B2)
  
}