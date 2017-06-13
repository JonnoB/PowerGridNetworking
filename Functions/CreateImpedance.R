CreateImpedance <- function(df){
  #takes a three column dataframe where the first two columns are the node that the edge will connect to
  #The third column is the impedance of the edge.
  #Returns an impedance matrix
  
  Impmat <- df[,1:2] %>% 
    as.matrix %>% 
    graph_from_edgelist(., directed = FALSE)
  
  E(Impmat)$weight <- as.numeric(unlist(df[,3]))
  
  Impmat <- as_adjacency_matrix(Impmat, attr = "weight") %>% as.matrix
  
  diag(Impmat ) <- -rowSums(Impmat )
  
  Impmat  <- -Impmat 
  return(Impmat)
}