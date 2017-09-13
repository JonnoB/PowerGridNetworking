RandomAttack <-function(g, Target = "Nodes", Number = 1){
  #The simplest strategy randomly attacks nodes in the network
  #g: a network an igraph object
  #Target: Either nodes or edges
  #Number the total number of nodes/edges to remove
  
  if(Target == "Nodes"){
    AllTargets <- V(g)$name
  } else if (Target =="Edges") {
    AllTargets <- E(g)$name
  } else {
    
    stop("Target must be either 'Nodes' or 'Edges'")
  }
  
  
  sample(AllTargets, Number)
}