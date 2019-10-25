#' Initiate power-grid attack simulation
#'
#' This function simulates an attack on the power grid using the parameter settings you choose
#'    the outut of the function is a nested list of igraph objects.
#' @param network_list A list of lists where each element of the sub list is an igraph object, the first time it is used the
#' the network list is simply list(list(g)).
#' @param attack_strategy A function that calculates which node to delete the function is is in "quo" form and embedded in an
#' attack type.
#' @param reference_grid The grid that will be used to test the largest component against if NULL it uses the given network.
#' @param min_max_comp The minimum size of the maximum component, as a fraction, for the process to continue, the default is set
#' to 0.0 complete collapse.
#' @param total_attack_rounds The maximum number of nodes to be removed before the process stops.
#' @param cascade_mode Whether the power flow equations will be used to check line-overloading or not.
#' @param cumulative_attacks  The total number of attacks that have taken place so far.
#' @param demand the node_name of the node Load variable. A character string.
#' @param generation the node_name of the node generation variable. A character string.
#' @param edge_name the variable that holds the edge names, a character string.
#' @param node_name the variable that holds the names of the nodes, to identify the slack ref. a character string
#' @param net_generation the node_name that the net generation data for each node is held in
#' @export
#' @examples
#' attack_the_grid(network_list, attack_strategy, SubstationData, EdgeData, reference_grid = NULL, min_max_comp = 0.8)
#' Out <- attack_the_grid(network_list,
#' attack_strategy,
#' reference_grid = NULL,
#' min_max_comp = 0.8,
#' total_attack_rounds=100,
#' cascade_mode = TRUE,
#' cumulative_attacks = NULL)
attack_the_grid <- function(network_list,
                          attack_strategy,
                          reference_grid = NULL,
                          min_max_comp = 0.0,
                          total_attack_rounds=1000,
                          cascade_mode = TRUE,
                          cumulative_attacks = NULL,
                          demand = "demand",
                          generation = "generation",
                          edge_name = "edge_name",
                          node_name = "name",
                          net_generation = "net_generation"){
  #gets the last network in the list
  gc()
  g <- network_list[[length(network_list)]]
  g <- g[[length(g)]]
  if(is.null(reference_grid)){
    reference_grid  <- g
  }
  #Remove the desired part of the network.
  gCasc <- attack_strategy %>% 
    eval_tidy(., data = list(g = g)) #The capture environment contains delete nodes, however the current g is fed in here
  #Rebalence network
  #This means that the cascade calc takes a balanced network which is good.
  gCasc <- balance_generation_and_demand(gCasc, demand, generation, output_var = net_generation)
  GridCollapsed <- ecount(gCasc)==0
  gCasc <- list(gCasc)
  if(is.null(cumulative_attacks)){
    cumulative_attacks2 <- 1
  } else {
    cumulative_attacks2 <- cumulative_attacks + 1
  }
  #This If statement prevents Cascading if theire are no cascadable components
  if(!GridCollapsed){
    if(cascade_mode){
      #this returns a list of networks each of the cascade
      gCasc <- cascade(network_list  = gCasc,
                       iteration = 0,
                       stop_cascade = Inf,
                       g_0 = g,
                       demand = demand,
                       generation = generation,
                       edge_name = edge_name,
                       node_name = node_name,
                       net_generation = net_generation)
    }
    message(paste("Attack ",cumulative_attacks2, " nodes Remaining", vcount(gCasc[[length(gCasc)]])))
  } else{
    message("Grid collapsed simulation complete")
  }
  #concatanate the new list with the list of lists
  network_list2 <- network_list
  network_list2[[length(network_list2)+1]] <-gCasc
  #extract the last network from the just completed cascade
  gCascLast <- gCasc[[length(gCasc)]]
  #If the largest componant is larger than the min_max_comp threshold
  #call the function again and delete a new node.
  #when the grid has collapsed problems arise this helps deal with that
  MaxComp <- suppressWarnings(max(components(gCascLast)$csize))
  #Checks to see if the topology of the network is unchanged.
  #If this is TRUE then nothing is being removed and the process can stop
  TopoStability <- (vcount(gCascLast) == vcount(g) &   ecount(gCascLast) == ecount(g))
  FractGC <-ifelse(is.finite(MaxComp),MaxComp/vcount(reference_grid), 0)
  #These conditions can probably be simplified a bit
  if( !(FractGC < min_max_comp | length(network_list2)-1==total_attack_rounds| GridCollapsed| TopoStability) ){
    network_list2 <- attack_the_grid(network_list = network_list2,
                                  attack_strategy,
                                  reference_grid = reference_grid,
                                  min_max_comp = min_max_comp,
                                  total_attack_rounds = total_attack_rounds,
                                  cascade_mode = cascade_mode,
                                  cumulative_attacks = cumulative_attacks2,
                                  demand = demand,
                                  generation = generation,
                                  edge_name = edge_name,
                                  node_name = node_name,
                                  net_generation = net_generation
    )
  }
  gc()
  return(network_list2)
}
