Net_Ability <- function(g, gOrig = NULL){
  #Calculates the PowerFlow from a graph that contains the following attributes
  #named edges, edgeweights, a balanced power generation and demand column, Powerflow (will be overwritten)
  #g: an igraph object
  #SlackRef: the node to remove from the calculations to prevent the matrix being singular
  
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
