#' Save attack simulations to folder
#'
#'When creating many attack simulations it can be conveniant to save the resulting graphs to a folder
#'    so that re-runs are not required if further analysis is needed. This function saves the
#'    results of attack_the_grid, and also allows the process to be interrupted.
#'    When the process is started again after and interuption the function finds the last
#'    save attack and starts the simulation process from there.
#' @param g An igraph object representing a power grid.
#' @param attack_vectors A dataframe. This is the output of the multi_attack_order function. If you expect.
#'    to interrupt the function this dataframe should be saved or the random seed used to generate the dataset
#'    recorded.
#' @param folder the folder data will be saved to.
#' @param min_max_comp See attack_the_grid. Default is 0
#' @param total_attack_rounds See attack_the_grid. Default is 10
#' @param cascade_mode See attack_the_grid. Default is set to FALSE
#' @param demand the node_name of the node Load variable. A character string.
#' @param generation the node_name of the node generation variable. A character string.
#' @param edge_name the variable that holds the edge names, a character string.
#' @param node_name the variable that holds the names of the nodes, to identify the slack ref. a character string
#' @param net_generation the node_name that the net generation data for each node is held in
#' @param target whether nodes or edges are being attacked
#' @keywords multi-attack,
#' @export
#' @seealso \code{\link{attack_the_grid}}, \code{\link{multi_attack_order}}
#' @examples
#' save_multi_attacks(g, attack_vectors, folder, cascade_mode = F)

save_multi_attacks <-  function(g, attack_vectors,
                              folder,
                              min_max_comp = 0,
                              total_attack_rounds = 10,
                              cascade_mode = FALSE,
                              demand = "demand",
                              generation = "generation",
                              edge_name = "edge_name",
                              node_name = "name",
                              net_generation = "net_generation",
                              target = "nodes"){
  
  #set working directory
  gc()
  print("Calculating first simulation")
  TimeAtFirstSimulation <- Sys.time()
  for (i in 1:nrow(attack_vectors)) {
    #walk was used previously but it created a crash.
    #The crash would happen approx every 30 simulations
    #I can't replicate the crash using dummy data so am leaving it.
    
    NextSim <- next_attack_simulation(attack_vectors, folder)
    
    if (NextSim=="Simulation_ID_Inf") break #Stops function making error on last iteration
    
    #Does not appear to actually print the next node
    #print(paste("Next target is", NextSim))
    
    node_name <- ifelse(target == "nodes", node_name, edge_name)
    
    
    deletion_order <- generate_attack_order(attack_vectors, folder)
    
    
    FixedNodes <- quo(fixed_strategy_attack(g, deletion_order, target, node_name))
    T1 <- Sys.time()
    #suppres attack the grid messages
    attack_series <-suppressMessages(attack_the_grid(network_list = list(list(g)),
                                                  attack_strategy = FixedNodes,
                                                  reference_grid = NULL,
                                                  min_max_comp,
                                                  total_attack_rounds,
                                                  cascade_mode,
                                                  demand = demand,
                                                  generation = generation,
                                                  edge_name = edge_name,
                                                  node_name = node_name,
                                                  net_generation = net_generation))
    
    saveRDS(attack_series, file = file.path(folder, paste0(NextSim, ".rds")))
    rm(attack_series)
    gc()
    #Sort out time stuff
    T2 <- Sys.time()
    SimulationRoundTime <- round(difftime(T2, T1, units = "mins" ))
    TimeToCompletion <- (difftime(T2, TimeAtFirstSimulation)/i)*(nrow(attack_vectors)-i)
    ExpectedCompletionTime<- T2 + TimeToCompletion
    TimeUnit<- ifelse(as.numeric(TimeToCompletion, units= "hours")<1, "mins", "hours")
    
    print(paste("Time taken for simulation", i, "is",
                SimulationRoundTime,
                "minutes. Est time to completion",
                round(as.numeric(TimeToCompletion, units = TimeUnit)), #
                TimeUnit,
                "Est completion time is",
                ExpectedCompletionTime))
  }
  TimeAtLastSimulation <- Sys.time()
  
  print(paste("Time taken for all simulations is", round(difftime(TimeAtLastSimulation, TimeAtFirstSimulation , units = "hours" ),2), "hours"))
  
}
