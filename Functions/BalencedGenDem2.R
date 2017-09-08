BalencedGenDem2 <- function(g, ValidNodes, DemandVar, GenerationVar, OutputVar = "NetPower"){
  #Balences the generation and demand across multiple isolated componants
  #g: the graph of the network that is being attacked
  #ValidNodes: the data frame of valid nodes and their demand and generation
  #DemandVar: The Variable name to be used for calculating nodal demand is bare and unquoted
  #OutputVar: The name of the variable that will be the new balanced power this is a character string
  
  DemandVar <- enquo(DemandVar)
  GenerationVar <- enquo(GenerationVar)
  
  # componentmatcher <- data_frame(Bus.Name = names(components(g)$membership), 
  #                                component = components(g)$membership)
  
  
  ValidNodesCasc2 <- ValidNodes %>%
   # left_join(componentmatcher, by = "Bus.Name") %>%
    group_by(component) %>%
    #Identify Deadislands and set Generation and Demand to 0
    mutate(DeadIsland = sum(!!DemandVar)==0 | sum(!!GenerationVar)==0,
           Demand2 = ifelse(DeadIsland, 0, !!DemandVar),
           Generation2 = if_else(DeadIsland, 0, !!GenerationVar)) %>%
    #rebalence remaining componants
    mutate(GenBal = ifelse(is.finite(sum(Demand2)/sum(Generation2)),sum(Demand2)/sum(Generation2),1),
           Demand3 = ifelse(GenBal>1,
                            Demand2*(sum(Generation2)/sum(Demand2)), Demand2),
           Generation3 = ifelse(GenBal<1,
                                Generation2*(sum(Demand2)/sum(Generation2)), Generation2),
           
           !!OutputVar := Generation3 - Demand3) %>%
    ungroup %>%
    select(-Demand2, -Generation2, -GenBal, -Demand3, -Generation3)
  
}