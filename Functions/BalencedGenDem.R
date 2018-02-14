BalencedGenDem <- function(g, DemandVar, GenerationVar, OutputVar = "BalencedPower"){
  #Balences the generation and demand across multiple isolated componants and removes all dead islands
  #the function takes a graph as an argument and outputs a graph
  #g: the graph of the network will have power balanced
  #DemandVar: The Variable name to be used for calculating nodal demand, character vector
  #GenerationVar: The Variable name to be used for calculating nodal Generation, character vector
  #OutputVar: The name of the variable that will be the new balanced power this is a character string
  
  df <- tibble(
    Demand = get.vertex.attribute(g, DemandVar) , 
    Generation = get.vertex.attribute(g, GenerationVar),
    component = components(g)$membership) %>%
    group_by(component) %>%
    #Identify Deadislands and set Generation and Demand to 
    #Net generation is used as this minimises network strain when there is a node
    #that prodices and consumes power. This can occure for an induced subgraph
    mutate(DeadIsland = sum(Demand)==0 | sum(Generation)==0,
           NetGen = Generation - Demand,
           Demand2 = ifelse(!DeadIsland & NetGen < 0, abs(NetGen), 0),
           Generation2 = if_else(!DeadIsland & NetGen > 0, NetGen, 0 )) %>%
    #rebalence remaining components
    mutate(GenBal = ifelse(is.finite(sum(Demand2)/sum(Generation2)),sum(Demand2)/sum(Generation2),1),
           Demand3 = ifelse(GenBal>1,
                            Demand2*(sum(Generation2)/sum(Demand2)), Demand2),
           Generation3 = ifelse(GenBal<1,
                                Generation2*(sum(Demand2)/sum(Generation2)), Generation2),
           Output = Generation3 - Demand3
    ) %>%
    ungroup
  
  g2 <- set.vertex.attribute(g, OutputVar, value =df$Output )
  g2 <- delete.vertices(g2, df$DeadIsland)
  return(g2)
  
  
}