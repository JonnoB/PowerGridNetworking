BetweenessAttack <-function(g, Target = "Nodes", Number = 1){
  #Attacks the nodes based on their betweenness, as long as the the graph has not weighted edges called "weights"
    #the betweeness is unweighted
  #g: a network an igraph object
  #Target: Either nodes or edges
  #Number the total number of nodes/edges to remove

  if(Target == "Nodes"){
    df <- as_data_frame(g, what = "vertices") %>%
      mutate(metric = betweenness(g)) %>%
      arrange(desc(metric))
    
    Out <- df$name[1:Number]
    
  } else if (Target =="Edges") {
    df <- as_data_frame(g, what = "edges") %>%
      mutate(metric = edge_betweenness(g)) %>%
      arrange(desc(metric))
    
    Out <- df$name[1:Number]
  } else {
    
    stop("Target must be either 'Nodes' or 'Edges'")
  }
  
  return(Out)
}
