multi_attack <- function(g, LoadProfiles, 
                             attack_strategy, 
                             type ="Fixed", 
                             min_max_comp, 
                             total_attack_rounds, 
                             target = "nodes", 
                             cascade_mode,
                             SaveGridList = NULL,
                             cores = 1){
  #This function performs a montevcarlo simulation of attack the grid. It out puts a dataframe of graph statistics and the full
  #deletion order for each simulation. The deletion order is the order in which the nodes were actually deleted, which is 
  #different to the order they were planned to be deleted. Some nodes will be removed due to cascade effects or, 
  #dead islanding. These nodes are then removed from the original deletion list and the next available node is deleted.
  #g: an igraph objects
  #LoadProfiles: A dataframe of load profiles across the network, a simulation will be done for each load profile
  #Attackstrategy: The attack strategy expression, simply type in the node_name of the function without brackets e.g "RandomAttack"
  #min_max_comp: The minimum size of the maximum component, as a fraction, for the process to continue, the default is set to 0.8
  #total_attack_rounds: The maximum number of nodes to be removed before the process stops
  #target: wheather nodes or edges are being deleted
  #cascade_mode: Whether the power flow equations will be used to check line-overloading or not
  #SaveGridList: A folder node_name to store the GridLists in will overwrite a folder of the same node_name
  #cores: the number of cores to use, 1 is defualt and runs in parallel. all other numbers use a doMC back end. 
    #currently doesn't run on windows
  #Calling gc prevents memory being eating up over the course of the simulation
  #It is called first to clean from the previous iterations memory use
  gc()
  Outlist <- list()
  if(!(type == "Flexible"| type == "Fixed"| type == "Adaptive")){
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
                  Loadn <- data_frame(node_name = get.vertex.attribute(g, "name")) %>%
                    left_join(., LoadProfiles[,c(1,n+1)], by = "name")
                  #Set new Load profile
                  g <- set.vertex.attribute(g, "demand", value = unlist(Loadn[,2]))
                   #If the power limits are being used these need to be calculated
                  if(cascade_mode){
                  #Balance the grid
                  g <- balance_generation_and_demand(g, "demand", "generation")
                  #Calculate power flows
                  g <- calc_over_limit(g)
                  }
                  #Calculate PowerFLow
                  #Choose the appropriate method for fixed or adaptive
                  if(type == "Flexible"){    
                    DeleteNodes <- attack_strategy(g, target, ifelse(target == "nodes", vcount(g), ecount(g)))
                    RemoveStrategy <- quo(fixed_strategy_attack(g, DeleteNodes, UQS(list(target = target))))
                  }else if(type == "Fixed"){    
                    DeleteNodes <- attack_strategy(g, target, total_attack_rounds)
                                   RemoveStrategy <- quo(fixed_strategy_attack(g, DeleteNodes, UQS(list(target = target))))
                  }else { #Adaptive
                    RemoveStrategy <- quo(adaptive_strategy_attack(g, attack_strategy, UQS(list(target = target))))
                  }
                  GridList <- attack_the_grid(network_list = list(list(g)), 
                                            attack_strategy =  RemoveStrategy, 
                                            min_max_comp = min_max_comp,
                                            total_attack_rounds = total_attack_rounds,
                                            cascade_mode =  cascade_mode) 
                  #save file to created folder if applicable
                  if(!is.null(SaveGridList)){
                    saveRDS(GridList, file = file.path(getwd(), SaveGridList, paste0(sim, ".RDS")))
                  }
                  Netstats <- GridList%>%
                    extract_network_stats(.) %>%
                    mutate( Simulation = n)
                  NodesAttacked <- nodes_targeted(GridList)
                  list(NetStats = Netstats , nodes_targeted = NodesAttacked)
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
    mutate(cascade = cascade_mode)
  #Combine all the deleted nodes into a single list
  AttackedNodes <- Outlist %>%
    modify_depth(., 1, keep, is.character ) %>%
    flatten  %>% flatten
  Outlist <- list(NetData = NetData, AttackedNodes = AttackedNodes)
  return(Outlist)
}
