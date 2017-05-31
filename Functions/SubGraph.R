SubGraph <- function(Name){
  Thegraph <-bind_rows(Circuits, Transformers) %>%  
    filter(grepl(Name, Node1)|grepl(Name,Node2)) %>%
    .[,1:2] %>%
    as.matrix() %>% 
    graph_from_edgelist(., directed = FALSE)
  
  plot(Thegraph)
  return(Thegraph)
}