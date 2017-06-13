CreateTransmission <- function(df, StartNode, EndNode, edgeweight){
  
  df <- df %>% rename_(Node1 = StartNode,
                       Node2 = EndNode,
                       EdgeWeight = edgeweight) %>%
    #mutate(Edgename = paste(Node1,Node2, sep = "-"))
    group_by(Node1, Node2) %>% #trying to stop the non-unique identifier problem
    mutate(Edgename = paste(Node1,Node2, 1:n(),sep = "-")) %>% #This allows multiple edges 
    #between the same node pair, it is not certain the data is always correct!
    ungroup %>%
    filter(Node1 !=Node2) #remove self loops
  
  df2 <- df %>% select(Edgename, Node1, EdgeWeight) 
  df3 <- df %>% select(Edgename, Node2, EdgeWeight) %>%
    mutate(EdgeWeight = -EdgeWeight) %>%
    rename(Node1 = Node2)
  
  df <- bind_rows(df2, df3) %>%
  spread(., Node1, EdgeWeight, fil = 0)
  
  Transmat <- as.matrix(df[,-1])
  rownames(Transmat)<- df$Edgename

  return(Transmat)
}