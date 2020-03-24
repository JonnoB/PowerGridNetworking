#' Network co-failure
#' 
#' This function finds the co-failure and failure mode of the node and edges in the attack on the network
#' 
#' This function produces a 6 element list. The node co failure matrix, the edge co failure matrix, the co-failure matrices for both
#' edge and node but excluding targeting. The final two elements are dataframes of the failure mode for the edges and nodes
#' 
#' @param collapse_list A list. This is the output of the attack_the_grid function
#' @export
network_co_failure <- function(collapse_list){
  
  node_failure_round <- !is.finite(collapse_list$node_power) & !is.na(collapse_list$node_power)
  node_co_failure_mat <- (node_failure_round %*% t(node_failure_round))
  
  #By removing all na values in the second logical expression of node_failure_round, the 2nd logical expression here doesn't return NA.
  node_failure_round_no_target <- node_failure_round  & collapse_list$node_power>0
  node_co_failure_no_target_mat <- (node_failure_round_no_target %*% t(node_failure_round_no_target))

  #The round the node is lost has to be done by row.
  #I don't think a vectorised version is possible
  node_failure_round_vect <- 1:nrow(node_failure_round) %>%
    map_int(~{
      
      round_lost <- which(node_failure_round[.x,])
      #nodes which survived until the complete collapse of the grid get an NA score
      ifelse(length(round_lost)>0, round_lost, NA )
    })
  
  #row sum is the way to go for extracting the node failure mode
  node_failure_temp <- rowSums(collapse_list$node_power, na.rm = T)
  #Any nodes left over once all edges are gone has self powering
  #Nodes that are still active will have the sum of their round power as final value not an infinite value.
  #All non-infinate values can be replaced by 0, and thus creating at max 3 factor levels
  node_failure_temp[is.finite(node_failure_temp)] <- 0
  node_failure_mode <- node_failure_temp 
  
  edge_failure_round <- collapse_list$edge_status > 0 & !is.na(collapse_list$edge_status)
  edge_co_failure <- edge_failure_round %*% t(edge_failure_round)
  
  #the same as in general co-failure but excluding targetted nodes.
  edge_failure_round_no_target <- edge_failure_round & collapse_list$edge_status!=1
  edge_co_failure_no_target <- edge_failure_round_no_target %*% t(edge_failure_round_no_target)
  
  #The round each edge was lost.
  edge_failure_round_vect <- 1:nrow(edge_failure_round) %>%
    map_int(~which(edge_failure_round[.x,]))
  
  #Edge failure is easier to find as the status is explicitly coded and all active states are 0 all missing states are NA
  #This means a row sum will find the failure mode of every edge
  edge_failure_mode <-rowSums(collapse_list$edge_status, na.rm = T)
  
  edge_failure_mode_df <- tibble(name = rownames(edge_failure_round),
         mode = factor(edge_failure_mode, levels = c(0:3), labels = c("Survived", "Targeted", "Overloaded", "Islanded")),
         round = edge_failure_round_vect)
  
  node_failure_mode_df <- tibble(name = rownames(node_failure_round),
                                 mode = factor(node_failure_mode, levels = c(-Inf, 0, Inf), labels = c("Targeted", "Survived" ,"Islanded")),
                                 round = node_failure_round_vect)
  #outputs a six element list
  return(list(node_co_failure = node_co_failure_mat,
              node_co_failure_no_target = node_co_failure_no_target_mat,
              edge_co_failure = edge_co_failure,
              edge_co_failure_no_target = edge_co_failure_no_target,
              edge_failure_mode = edge_failure_mode_df,
              node_failure_mode = node_failure_mode_df))
  
  
}