CreateTransmission <- function(df, StartNode, EndNode, edgeweight = NULL){
  
  #allows weighted and unweughted matrices
  if(is.null(edgeweight)){
    df<- df %>% rename_(Node1 = StartNode,
                        Node2 = EndNode) %>%
      mutate(EdgeWeight = 1)
  } else{
    
    df<- df %>% rename_(Node1 = StartNode,
                        Node2 = EndNode,
                        EdgeWeight = edgeweight)
  }
  
  df <- df %>%
    #mutate(Edgename = paste(Node1,Node2, sep = "-"))
    group_by(Node1, Node2) %>% #trying to stop the non-unique identifier problem
    mutate(Edgename = paste(Node1,Node2, 1:n(),sep = "-")) %>% #This allows multiple edges 
    #between the same node pair, it is not certain the data is always correct!
    ungroup %>%
    filter(Node1 !=Node2) #remove self loops
  
  df2 <- df %>% select(Edgename, Node1, EdgeWeight) 
  df3 <- df %>% select(Edgename, Node2, EdgeWeight) %>%
    #sets the negative edgeweight
    mutate(EdgeWeight = -EdgeWeight) %>%
    rename(Node1 = Node2)
  
  df <- bind_rows(df2, df3) %>%
  spread(., Node1, EdgeWeight, fil = 0)
  
  Transmat <- as.matrix(df[,-1])
  rownames(Transmat)<- df$Edgename
  
  #order the matrix alphabetically
  Transmat <- Transmat[order(rownames(Transmat)), order(colnames(Transmat))]

  return(Transmat)
}