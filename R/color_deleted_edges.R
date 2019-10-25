ColourDeletedEdges <- function(g_1, g_2){
  #This function takes two graphs that are seperated by a time step and colours the edges that will be deleted
  #in the followinf time step. It takes two igraph networks as arguements, these networks must be the same except one has less nodes/edges.
  #g_1: Graph whose edges will be coloured
  #g_2: Graph which has had edges or nodes deleted   
  edge1 <- get.edgelist(g_1) %>% 
    as_tibble %>%
    mutate(Link = paste(V1, V2))
  edge2 <- get.edgelist(g_2) %>% 
    as_tibble %>%
    mutate(Link = paste(V1, V2))
  E(g_1)$color <- ifelse(edge1$Link %in% edge2$Link, "black", "red")
  return(g_1)
} 
