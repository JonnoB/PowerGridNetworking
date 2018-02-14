MultiAttack <- function(g, LoadProfiles, 
                             AttackStrategy, 
                             Type ="Fixed", 
                             MinMaxComp, 
                             TotalAttackRounds, 
                             Target = "Nodes", 
                             CascadeMode,
                             SaveGridList = NULL,
                             cores = 1){
  #This function performs a montevcarlo simulation of attack the grid. It out puts a dataframe of graph statistics and the full
  #deletion order for each simulation. The deletion order is the order in which the nodes were actually deleted, which is 
  #different to the order they were planned to be deleted. Some nodes will be removed due to cascade effects or, 
  #dead islanding. These nodes are then removed from the original deletion list and the next available node is deleted.
  
  #g: an igraph objects
  
  #LoadProfiles: A dataframe of load profiles across the network, a simulation will be done for each load profile
  
  #Attackstrategy: The attack strategy expression, simply type in the name of the function without brackets e.g "RandomAttack"
  #MinMaxComp: The minimum size of the maximum component, as a fraction, for the process to continue, the default is set to 0.8
  #TotalAttackRounds: The maximum number of nodes to be removed before the process stops
  #Target: wheather nodes or edges are being deleted
  #CascadeMode: Whether the power flow equations will be used to check line-overloading or not
  #SaveGridList: A folder name to store the GridLists in will overwrite a folder of the same name
  #cores: the number of cores to use, 1 is defualt and runs in parallel. all other numbers use a doMC back end. 
    #currently doesn't run on windows
  
  #Calling gc prevents memory being eating up over the course of the simulation
  #It is called first to clean from the previous iterations memory use
  gc()
  
  Outlist <- list()
  
  if(!(Type == "Flexible"| Type == "Fixed"| Type == "Adaptive")){
    message("Type must be 'Fixed', 'Flexible oPr 'Adaptive'")
    stop()
  }
  
  if(!is.null(SaveGridList)){
    unlink(file.path(getwd(), SaveGridList), recursive = TRUE)
    dir.create(file.path(getwd(), SaveGridList), showWarnings = FALSE)
  }
  
  
  #Quoting this expression allows the for loop to be easily paralellised
  InsideForLoop <- quo(   { 
                  sim <- paste0("simulation_", n)
                  print(sim)
                  
                  #Load Profile assigned here
                  #make sure loadprofile is in the right order
                  Loadn <- data_frame(name = get.vertex.attribute(g, "name")) %>%
                    left_join(., LoadProfiles[,c(1,n+1)], by = "name")
                  #Set new Load profile
                  g <- set.vertex.attribute(g, "Demand", value = unlist(Loadn[,2]))
                  
                   #If the power limits are being used these need to be calculated
                  if(CascadeMode){
                  #Balance the grid
                  g <- BalencedGenDem(g, "Demand", "Generation")

                  #Calculate power flows
                  g <- CalcOverLimit(g)
                  }
                  
                  #Calculate PowerFLow
                  
                  #Choose the appropriate method for fixed or adaptive
                  if(Type == "Flexible"){    
                    
                    DeleteNodes <- AttackStrategy(g, Target, ifelse(Target == "Nodes", vcount(g), ecount(g)))
                    RemoveStrategy <- quo(FixedStrategyAttack(g, DeleteNodes, UQS(list(Target = Target))))
                    
                  }else if(Type == "Fixed"){    
                    
                    DeleteNodes <- AttackStrategy(g, Target, TotalAttackRounds)
                                   RemoveStrategy <- quo(FixedStrategyAttack(g, DeleteNodes, UQS(list(Target = Target))))
                                                  
                  }else { #Adaptive
                    RemoveStrategy <- quo(AdaptiveStrategyAttack(g, AttackStrategy, UQS(list(Target = Target))))
                  }
                  
                  GridList <- AttackTheGrid(NetworkList = list(list(g)), 
                                            AttackStrategy =  RemoveStrategy, 
                                            MinMaxComp = MinMaxComp,
                                            TotalAttackRounds = TotalAttackRounds,
                                            CascadeMode =  CascadeMode) 
                  
                  #save file to created folder if applicable
                  if(!is.null(SaveGridList)){
                    saveRDS(GridList, file = file.path(getwd(), SaveGridList, paste0(sim, ".RDS")))
                  }
                  
                  Netstats <- GridList%>%
                    ExtractNetworkStats(.) %>%
                    mutate( Simulation = n)
                  
                  NodesAttacked <- NodesTargeted(GridList)
                  
                  list(NetStats = Netstats , NodesTargeted = NodesAttacked)
                  }
    )
  
  if(cores ==1 ){
    
    for(n in 1:(ncol(LoadProfiles)-1)){ # ready for paralellization
      Outlist[[n]]<- eval_tidy( InsideForLoop, data = list(n=n))
    }
    
  } else{
    print(cores)
    registerDoMC(cores) 
    Outlist <- foreach(iter = 1:(ncol(LoadProfiles)-1)) %dopar% {
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

