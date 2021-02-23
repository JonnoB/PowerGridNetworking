ColourDeletedEdges <- function(g1, g2){
  #This function takes two graphs that are seperated by a time step and colours the edges that will be deleted
  #in the followinf time step. It takes two igraph networks as arguements, these networks must be the same except one has less nodes/edges.
  #g1: Graph whose edges will be coloured
  #g2: Graph which has had edges or nodes deleted
  edge1 <- igraph::get.edgelist(g1) %>%
    tibble::as_tibble %>%
    dplyr::mutate(Link = paste(V1, V2))

  edge2 <- igraph::get.edgelist(g2) %>%
    tibble::as_tibble %>%
    dplyr::mutate(Link = paste(V1, V2))

  igraph::E(g1)$color <- ifelse(edge1$Link %in% edge2$Link, "black", "red")

  return(g1)

}
