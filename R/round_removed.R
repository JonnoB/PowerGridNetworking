#' Find Deletion order
#'
#' Attack round that each edge/node was deleted is  extracted from the network List of lists produced by attack_the_grid.
#' Whether the node/edge was the targeted node/edge is also marked. This currently only works for nodes not Edges
#'
#' If not all edges/nodes were deleted during attack_the_grid, these edges will be filled with NA
#'
#' @param network_list The output of attack_the_grid, a list of lists containing igraphs.
#' @param type Whether Edge or node order will be found. The default is Edge, any other value will mean nodes are used.
#' @export
#' @seealso \code{\link[PowerGridNetworking]{attack_the_grid}}
round_removed <- function(network_list, type = "edge"){
  if(type =="edge"){
    Removetype <- function(ListHere){
      ListHere %>% edge_attr(., "edge_name")
    }
  } else if (type =="Node") {
    Removetype <- function(ListHere){
      ListHere %>% vertex_attr(., "name")
    }
  } else {
    stop("type must be either 'Edge' or 'Node'")
  }
  RemovedDF <- data_frame(Names = Removetype(network_list[[1]][[1]]),
                          round_removed = NA,
                          RemovalType = "Unknown")
  #find the final graph in each attack round
  FinalGraph <-network_list %>%
    map_dbl(~length(.x))
  #cycle through each attack round
  for(i in 2:length(FinalGraph)){
    TargetNodeName <- network_list[[i]][[FinalGraph[i]]] %>%
      get.graph.attribute() %>%
      .$Removed
    NamesRemoved <- difference(network_list[[i-1]][[FinalGraph[i-1]]],
                               network_list[[i]][[FinalGraph[i]]])  %>%
      Removetype()
    RemovedDF <- RemovedDF %>%
      mutate(round_removed = ifelse(Names %in% NamesRemoved, i-1, round_removed))
    #Only if the target is edges
    OverloadedEdges <- network_list[[i]][[FinalGraph[i]]] %>%
      get.graph.attribute() %>%
      .$EdgesOverloaded
    RemovedDF <- RemovedDF %>%
      #Chooses entry by logically excluding options
      mutate(RemovalType= case_when(
        type == "edge" & Names %in% OverloadedEdges ~ "Overloaded",
        Names %in% TargetNodeName ~ "Targeted",
        TRUE ~ RemovalType
      ))
  }
  return(RemovedDF)
}
