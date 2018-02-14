Net_Ability <- function(g, gOrig = NULL){
#This function calculates the neta-bility of a graph.
  #The algorithm was implemented from "power grids vulnerability a complex networks approach" Arianos 2009 and
  #"Analysis of structural vulnerabilities in power transmission grids" Bompard 2009.
  #g: is the graph to have netability calculated
  #gOrig: is the original graph structure before failure, this is important as if Generation or demand nodes are removed
  #from the graph due to failure they will not be considered in the net-ability potentially increasing new graphs score as
  #the total number of node pairs will be smaller in the newgraph. Because of this the number of node pairs always has to be 
  #calculated from the original graph
  
  
  if(is.null(gOrig)){
    gOrig <- g
  }
  
  #finds the slack reference in each component
  SlackRefCasc <-  tibble(name = get.vertex.attribute(g, "name"),
                          Bus.Order = get.vertex.attribute(g, "Bus.Order"),
                          component = components(g)$membership) %>%
    group_by(component) %>%
    arrange(Bus.Order) %>%
    summarise(name = first(name),
              Nodes = n())
  
  message(paste("Number of componants", max(SlackRefCasc$component)))
  
  #calculates ratio of the electrical distance and the capacity matrix 
    Numerator <- 1:nrow(SlackRefCasc) %>%
    map(~{
          SlackRef <- SlackRefCasc %>% slice(.x)
          message("Component", .x)
          
          gsubset <- delete.vertices(g, components(g)$membership != .x)
          
          lemma <- ImpPTDF(gsubset, SlackRef$name)
          
          message("Calculating the electrical distance") #these two can take a long time so a message is given
          
          ElecDist <- ElectricalDistance(lemma$Imp)
          
          message("Calculating the Capacity Matrix")
          
          CapacityMatrix <- TransferCapacity(gsubset, lemma$PTDF) 
          
          Numerator <- CapacityMatrix/ElecDist
          
          Numerator[!is.finite(Numerator)] <- 0
          
          Numerator <-sum(Numerator)
          
          return(Numerator)
     
    }) %>%
    unlist %>% 
    sum
  
  #gets the counts of the number of generator and load busses
  #This keeps the slack variable in. That may be not a good idea I am not sure
  GenAndDem <-data_frame(
    name = get.vertex.attribute(gOrig, "name"),
    type = case_when(
      get.vertex.attribute(gOrig, "Demand") > get.vertex.attribute(gOrig, "Generation") ~ "Demand",
      get.vertex.attribute(gOrig, "Demand") < get.vertex.attribute(gOrig, "Generation") ~ "Generation",
      TRUE ~"Transmission"
    )) %>%
    group_by(type) %>%
    summarise(counts = n()) %>%
    filter(type != "Transmission")
  
  Denominator <- prod(GenAndDem$counts)
  
  NetAb <- sum(Numerator)/Denominator
  
  return(NetAb)
}
