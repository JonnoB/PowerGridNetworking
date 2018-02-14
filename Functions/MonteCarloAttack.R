MonteCarloAttack <- function(g, simulations = 100, 
                             AttackStrategy, 
                             Type ="Fixed", 
                             MinMaxComp, 
                             TotalAttackRounds, 
                             Target = "Nodes", 
                             CascadeMode,
                             cores = 1){
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
  #cores: the number of cores to use, 1 is defualt and runs in parallel. all other numbers use a doMC back end. 
          #currently doesn't run on windows
  
  #Calling gc prevents memory being eating up over the course of the simulation
  #It is called first to clean from the previous iterations memory use
  gc()
  
  Outlist <- list()
  
  if(!(Type == "Fixed"| Type == "Adaptive")){
    message("Type must be 'Fixed' or 'Adaptive'")
    stop()
  }
  
  if(Type == "Fixed"){
  DeletionMatrix <- 1:simulations %>% map( ~
                                   AttackStrategy(g, Target, ifelse(Target == "Nodes", vcount(g), ecount(g)))
  )
  }
  
  #Quoting this expression allows the for loop to be easily paralellised
  InsideForLoop <- quo(   { sim <- paste0("Simulation_", n)
                           print(sim)
                           
                           #Choose the appropriate method for fixed or adaptive
                           if(Type == "Fixed"){    
                             
                             DeleteNodes <- DeletionMatrix[[n]]
                             RemoveStrategy <- quo(FixedStrategyAttack(g, DeleteNodes, UQS(list(Target = Target))))
                             
                           } else {
                             RemoveStrategy <- quo(AdaptiveStrategyAttack(g, AttackStrategy, UQS(list(Target = Target))))
                           }
                           
                           GridList <- AttackTheGrid(NetworkList = list(list(g)), 
                                                     AttackStrategy =  RemoveStrategy, 
                                                     MinMaxComp = MinMaxComp,
                                                     TotalAttackRounds = TotalAttackRounds,
                                                     CascadeMode =  CascadeMode) 
                      
                          Netstats <- GridList%>%
                             ExtractNetworkStats(.) %>%
                             mutate( Simulation = n)
                      
                          NodesAttacked <- NodesTargeted(GridList)
                           
                          list(NetStats = Netstats , NodesTargeted = NodesAttacked)
                          }
                           )
  
  if(cores ==1 ){
  
    for(n in 1:simulations){ # ready for paralellization
    Outlist[[n]]<- eval_tidy( InsideForLoop, data = list(n=n))
    }
    
  } else{
    print(cores)
    registerDoMC(cores) 
    Outlist <- foreach(iter = 1:simulations) %dopar% {
       eval_tidy( InsideForLoop, data = list(n=iter))
    }
    
    
  }

    #Combine all the simualtion network statistics into a single dataframe
  NetData <- Outlist %>%
    modify_depth(., 1, keep, is.data.frame ) %>%
    flatten %>%
    bind_rows() %>%
  mutate(Cascade = CascadeMode)

  #Combine all the deleted nodes into a single list
  AttackedNodes <- Outlist %>%
    modify_depth(., 1, keep, is.character ) %>%
    flatten  %>% flatten

  Outlist <- list(NetData = NetData, AttackedNodes = AttackedNodes)

  return(Outlist)
}

