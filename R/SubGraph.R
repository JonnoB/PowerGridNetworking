SubGraph <- function(Name){
  Thegraph <-dplyr::bind_rows(Circuits, Transformers) %>%  
    dplyr::filter(grepl(Name, Node1)|grepl(Name,Node2)) %>%
    .[,1:2] %>%
    as.matrix() %>% 
    igraph::graph_from_edgelist(., directed = FALSE)
  
  plot(Thegraph)
  return(Thegraph)
}
