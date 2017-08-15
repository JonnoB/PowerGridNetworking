LinePropertiesMatrix <- function(df, StartNode, EndNode){
  #This function created a line properties matrix that matches the output of the Transmission matrix function
  
  df <- df %>% rename_(Node1 = StartNode,
                       Node2 = EndNode) %>%
    #mutate(Edgename = paste(Node1,Node2, sep = "-"))
    group_by(Node1, Node2) %>% #trying to stop the non-unique identifier problem
    mutate(Edgename = paste(Node1,Node2, 1:n(),sep = "-")) %>% #This allows multiple edges 
    #between the same node pair, it is not certain the data is always correct!
    ungroup %>%
    filter(Node1 !=Node2) #remove self loops
  
  C <- matrix(data = 0, nrow = nrow(df), ncol = nrow(df))
  diag(C) <- Impedance$Y
  colnames(C)<- df$Edgename
  rownames(C)<- df$Edgename
  
  return(C)
}