CalcOverLimit <- function(g){
  #This function calculates which edges are over the limit for the current network configuration and demand
  #production profile.
  #g: a graph with multiple edge and vertex attributes. The graph is assumed to be balenced in demand and production.
  
  g2 <-g
  
  #finds the slack reference in each component
  SlackRefCasc <-  tibble(name = get.vertex.attribute(g2, "name"),
                          Bus.Order = get.vertex.attribute(g2, "Bus.Order"),
                          component = components(g2)$membership) %>%
    group_by(component) %>%
    arrange(Bus.Order) %>%
    summarise(name = first(name),
              Nodes = n())

  #Calculate power flow for each component of the network as seperate networks
  gOut <- 1:nrow(SlackRefCasc) %>%
    map(~{
      
      #print(paste("PowerFlow for componant", .x))
      
      SlackRef <- SlackRefCasc %>% slice(.x)
      
      g2subset <- delete.vertices(g2, components(g2)$membership != .x)
      
      if(SlackRef$Nodes > 1){
        
        g2subset <- PowerFlow(g2subset, SlackRef$name)
      }
      
      g2subset

    }) %>%
    Reduce(union, .)

    
  return(gOut)

}