CreateSubgraph <- function(g, Nodes){
  #This function creates a subgraph where the nodes which have lost links to outside the subgraph gain additional generation
  #capacity. This generation capacity is limited to the line limits of the PowerLines.
  #This limit prevents those lines from being overpowered which is a problem I can deal with later when it affects things
  #g: and igraph object
  #Nodes: A character vector of node names to be in the new graph.
  g %>% 
    set.edge.attribute(., "weight", value = get.edge.attribute(., "Link.Limit")) %>%
    set.vertex.attribute(., "OldStrength", value = strength(.)) %>%
    induced_subgraph(., Nodes) %>%
    set.vertex.attribute(., "StrengthChange", value = strength(.) - get.vertex.attribute(., "OldStrength")) %>%
    set.vertex.attribute(., "Generation", 
                         value = get.vertex.attribute(., "Generation") - get.vertex.attribute(., "StrengthChange")) %>%
    BalencedGenDem(., "Demand", "Generation")
  
  
}