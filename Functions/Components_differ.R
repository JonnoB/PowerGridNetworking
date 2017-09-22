Components_differ<-function(g, g0){
  #This function checks to see which subcomponents have changed in the graph and which are the same.
  #using this should be able to cut down on the amount of calculation done on the power flow equations
  #g the current graph
  #g0 the previous graph
  
  gcomps <- components(g)$no #current graph
  g0comps <- components(g0)$no #previous graph
  
  gcompsList <- 1:gcomps %>%
    map(~ {
      newgraph <- delete.vertices(g, components(g)$membership != .x)
      get.edge.attribute(newgraph)$name}
    )
  
  g0compsList <- 1:g0comps %>%
    map(~ {
      newgraph <- delete.vertices(g0, components(g0)$membership != .x)
      get.edge.attribute(newgraph)$name}
    )
  
  dat <- expand.grid(1:gcomps, 1:g0comps)
  dat <- map2_lgl(.x = dat$Var1, .y  = dat$Var2, ~ {
    isTRUE(all.equal(gcompsList[[.x]] ,g0compsList[[.y]]))
  }
  ) %>%
    matrix(data = . , nrow = gcomps) %>% 
    rowSums
  
  dat == 0
  
}