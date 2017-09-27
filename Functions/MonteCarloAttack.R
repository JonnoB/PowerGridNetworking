MonteCarloAttack <- function(g, simulations = 100, AttackStrategy, Type ="Fixed", MinMaxComp, TotalAttackRounds, Target = Nodes, CascadeMode ){
  #This function performs a montevcarlo simulation of attack the grid. It out puts a dataframe of graph statistics and the full
  #deletion order for each simulation. The deletion order is the order in which the nodes were actually deleted, which is 
  #different to the order they were planned to be deleted. Some nodes will be removed due to cascade effects or, 
  #dead islanding. These nodes are then removed from the original deletion list and the next available node is deleted.
  
  #g: an igraph objects
  #simulations: the total number of simulations to perform
  #Attackstrategy: The attack strategy expression, simply type in the name of the function without brackets e.g "RandomAttack"
  #MinMaxComp: The minimum size of the maximum component, as a fraction, for the process to continue, the default is set to 0.8
  #TotalAttackRounds: The maximum number of nodes to be removed before the process stops
  #Target: wheather nodes or edges are being deleted
  #CascadeMode: Whether the power flow equations will be used to check line-overloading or not
  Bigdf <- list()
  
  if(!(Type == "Fixed"| Type == "Adaptive")){
    message("Type must be 'Fixed' or 'Adaptive'")
    stop()
  }
  
  if(Type == "Fixed"){
  DeletionMatrix <- 1:simulations %>% map( ~
                                   AttackStrategy(g, Target, Number)
  )
  }

  for(n in 1:simulations){ # ready for paralellization
    
    #Choose the appropriate method for fixed or adaptive
    if(Type == "Fixed"){    
      DeleteNodes <- DeletionMatrix[[n]]
      RemoveStrategy <- quo(FixedStrategyAttack(g, DeleteNodes, UQS(list(Target = Target))))
      } else {
        RemoveStrategy <- quo(AdaptiveStrategyAttack(g, AttackStrategy, UQS(list(Target = Target))))
    }
    
    sim <- paste0("Simulation_", n)
    print(sim)

    GridList <- AttackTheGrid(list(list(g)), 
                              RemoveStrategy, 
                              MinMaxComp = MinMaxComp,
                              TotalAttackRounds = TotalAttackRounds,
                              CascadeMode =  CascadeMode) %>%
      ExtractNetworkStats(.) %>%
      mutate( Simulation = n)
    #Calling gc prevents memory being eating up over the course of the simulation
    gc()
    
    Bigdf[[n]] <- GridList
  }
  
  Bigdf <- Bigdf %>% bind_rows
  
  return(Bigdf)
}

