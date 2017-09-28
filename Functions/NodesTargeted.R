NodesTargeted <-function(NetworkList, DeleteNodes){
  #extracts the nodes deleted accounting for the fact that the deletion order may be messed up due to cascades
  #Networklist: a list of lists of networks. The output of Attack the grid
  #DeleteNodes: The order in which nodes will be deleted
  
  AllremovedNodes <- NetworkList %>%
    map2(.x =., 
         .y = map_dbl(.x = ., .f =length), 
         ~ {
           V(.x[[.y]])$name # extract names in each graph
         } ) 

  Out <- AllremovedNodes %>%
    length(.) %>% 2:. %>% 
    map_chr(~{
      
      
      DiffBetweenSteps <- !(AllremovedNodes[[.x-1]] %in% AllremovedNodes[.x])
      
      remainingNodes <- DeleteNodes %>%
        data_frame(Nodes = .) %>%
        mutate(order = 1:n()) %>%
        filter(Nodes %in% AllremovedNodes[[.x-1]][DiffBetweenSteps ])
      
      remainingNodes$Nodes[1]
      
    }
    )
  
  return(Out)
  
}