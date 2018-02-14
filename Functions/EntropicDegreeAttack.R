EntropicDegreeAttack <-function(g, Target = "Nodes", Number = 1){
  #This strategy starts with the highest entropicdegree nodes and removes tham in alpha-numeric order.
  #As degree is a function of nodes it can only be performed on vertices
  #g: a network an igraph object
  #Target: Either nodes or edges
  #Number the total number of nodes/edges to remove
  
  if(Target == "Nodes"){
    df <- as_data_frame(g, what = "vertices") %>%
      mutate(metric = EntropicDegree(g, Scale = TRUE)) %>%
      arrange(desc(metric))
    Out <- df$name[1:Number]
  } else {
    
    stop("Only Nodes available for the Degree attack strategy")
  }
  
  
  return(Out)
}