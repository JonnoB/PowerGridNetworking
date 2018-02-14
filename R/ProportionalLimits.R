ProportionalLimits <- function(g, alpha = 1.05, Link.Limit = "Link.Limit"){
  #Sets the line limits of the network to a value proportional to thier loading level.
  #g: an igraph object
  #alpha: The desired tolerance of the system
  #Link.Limit: The edge attribute that defines the maximum power flow of the network
  
  LineLimits <- CalcOverLimit(g) %>% 
    get.edge.attribute("PowerFlow") %>%
    abs(.)*alpha
  
  g2 <- set.edge.attribute(g, name = Link.Limit, value = LineLimits)
  
  return(g2)
}