RandomAttack <-function(g, Target = "Nodes", Number = 1){
  #The simplest strategy randomly attacks nodes in the network
  #g: a network an igraph object
  #Target: Either nodes or edges
  #Number the total number of nodes/edges to remove
  
  if(Target == "Nodes"){
    Out <- sample(V(g)$name, Number)
  } else if (Target =="Edges") {
    Out <- sample(E(g)$name, Number)
  } else {
    
    stop("Target must be either 'Nodes' or 'Edges'")
  }
  
  return(Out)
}