NodesTargeted <-function(NetworkList){
  #Gets the name of the deleted target node/edge from the named attribute in each attack list.
  #Networklist: a list of lists of networks. The output of Attack the grid

  Out <- NetworkList %>% 
    purrr::map(~ igraph::get.graph.attribute(.x[[1]], "Removed")) %>% 
    unlist(.) %>% 
    c("None", .) #adds on the first element where there was nothing deleted.
  return(Out)
  
}
