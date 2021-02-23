DegreeAttack <-function(g, Target = "Nodes", Number = 1){
  #This strategy starts with the highest degree nodes and removes tham in alpha-numeric order.
  #As degree is a function of nodes it can only be performed on vertices
  #g: a network an igraph object
  #Target: Either nodes or edges
  #Number the total number of nodes/edges to remove
  
  if(Target == "Nodes"){
    df <- igraph::as_data_frame(g, what = "vertices") %>%
      dplyr::mutate(metric = igraph::degree(g)) %>%
      dplyr::arrange(dplyr::desc(metric))
    Out <- df$name[1:Number]
  } else {
    
    stop("Only Nodes available for the Degree attack strategy")
  }
  

    return(Out)
}
