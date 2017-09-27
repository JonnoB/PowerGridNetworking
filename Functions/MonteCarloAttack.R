MonteCarloAttack <- function(g, simulations = 100, DeletionMatrix, MinMaxComp, TotalAttackRounds, CascadeMode ){
  
  Bigdf <- list()
  
  for(n in 1:simulations){ # ready for paralellization
    
    DeleteNodes <- DeletionMatrix[[n]]

    FixedNodes <- quo(FixedStrategyAttack(g, DeleteNodes))
    
    sim <- paste0("Simulation_", n)
    print(sim)

    GridList <- AttackTheGrid(list(list(g)), 
                                               FixedNodes, 
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