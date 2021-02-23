CreateImpedance <- function(df){
  #takes a three column dataframe where the first two columns are the node that the edge will connect to
  #The third column is the impedance of the edge.
  #Returns an impedance matrix
  
  Impmat <- df[,1:2] %>% 
    as.matrix %>% 
    igraph::graph_from_edgelist(., directed = FALSE)
  
  igraph::E(Impmat)$weight <- as.numeric(unlist(df[,3]))
  
  Impmat <- igraph::as_adjacency_matrix(Impmat, attr = "weight") %>% as.matrix
  
  diag(Impmat ) <- -rowSums(Impmat )
  
  Impmat  <- -Impmat
  
  #order the matrix alphabetically
  Impmat <-Impmat[order(rownames(Impmat)), order(colnames(Impmat))]
  
  return(Impmat)
}
