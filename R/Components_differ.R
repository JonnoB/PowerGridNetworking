Components_differ<-function(g, g0){
  #This function checks to see which subcomponents have changed in the graph and which are the same.
  #using this should be able to cut down on the amount of calculation done on the power flow equations
  #g the current graph
  #g0 the previous graph

  gcomps <- components(g)$no #current graph
  g0comps <- components(g0)$no #previous graph


  #Create a list of the nodes names in each component, for the current graph

  gcompsList <- 1:gcomps %>%
    map(~ {
      newgraph <- delete.vertices(g, components(g)$membership != .x)
      get.edge.attribute(newgraph)$name}
    )

  #Create a list of the nodes names in each component, for the previous
  g0compsList <- 1:g0comps %>%
    map(~ {
      newgraph <- delete.vertices(g0, components(g0)$membership != .x)
      get.edge.attribute(newgraph)$name}
    )

  #Make a matrix that allows each list to be checked against the other
  dat <- expand.grid(1:gcomps, 1:g0comps)


  dat <- map2_lgl(.x = dat$Var1, .y  = dat$Var2, ~ {
    base::all(g0compsList[[.y]] %in% gcompsList[[.x]]) #all nodes in component are equal
  }
  ) %>%
    matrix(data = . , nrow = gcomps) %>%
    rowSums

  dat == 0

}
