CalcOverLimit2 <- function(g, g0 = NULL){
  #This function calculates which edges are over the limit for the current network configuration and demand
  #production profile.
  #g: a graph with multiple edge and vertex attributes. The graph is assumed to be balenced in demand and production.
  #gComp: The previous graph in the series, it is used to check which components are the same as previously only components 
  #that change are recalculated, this will hopefull save time.
  
  
  if(is.null(g0)){
    differ <- rep(TRUE, components(g)$no)
  } else {
    differ <- Components_differ(g, g0)
  }
  
  #finds the slack reference in each component
  SlackRefCasc <-  tibble(name = get.vertex.attribute(g, "name"),
                          Bus.Order = get.vertex.attribute(g, "Bus.Order"),
                          component = components(g)$membership) %>%
    group_by(component) %>%
    arrange(Bus.Order) %>%
    summarise(name = first(name),
              Nodes = n()) %>%
    mutate(differ = differ)
  
  #Calculate power flow for each component of the network as seperate networks
  gOut <- 1:nrow(SlackRefCasc) %>%
    map(~{
      
      #print(paste("PowerFlow for componant", .x))
      
      SlackRef <- SlackRefCasc %>% slice(.x)
      
      g2subset <- delete.vertices(g, components(g)$membership != .x)
      
      if(SlackRef$Nodes > 1 && SlackRef$differ){
        
        g2subset <- PowerFlow(g2subset, SlackRef$name)
      }
      
      g2subset
      
    }) %>%
        Reduce(union2, .)

    
  return(gOut)

}