#' Find Deletion order
#'
#' Attack round that each edge/node was deleted is  extracted from the network List of lists produced by AttackTheGrid.
#' Whether the node/edge was the targeted node/edge is also marked. This currently only works for Nodes not Edges
#'
#' If not all edges/nodes were deleted during AttackTheGrid, these edges will be filled with NA
#'
#' @param ListofLists The output of AttackTheGrid, a list of lists containing igraphs.
#' @param type Whether Edge or node order will be found. The default is Edge, any other value will mean nodes are used.
#' @export
#' @seealso \code{\link[PowerGridNetworking]{attack_the_grid}}

RoundRemoved <- function(ListofLists, type = "Edge"){

  if(type =="Edge"){

    Removetype <- function(ListHere){
      ListHere %>% igraph::edge_attr(., "Link")
    }

  } else if (type =="Node") {
    Removetype <- function(ListHere){
      ListHere %>% igraph::vertex_attr(., "name")
    }
  } else {
    stop("type must be either 'Edge' or 'Node'")
  }

  RemovedDF <- dplyr::data_frame(Names = Removetype(ListofLists[[1]][[1]]),
                          RoundRemoved = NA,
                          RemovalType = "Unknown")

  #find the final graph in each attack round
  FinalGraph <-ListofLists %>%
    purrr::map_dbl(~length(.x))

  #cycle through each attack round
  for(i in 2:length(FinalGraph)){

    TargetNodeName <- ListofLists[[i]][[FinalGraph[i]]] %>%
      igraph::get.graph.attribute() %>%
      .$Removed

    NamesRemoved <- igraph::difference(ListofLists[[i-1]][[FinalGraph[i-1]]],
                               ListofLists[[i]][[FinalGraph[i]]])  %>%
      Removetype()

    RemovedDF <- RemovedDF %>%
      dplyr::mutate(RoundRemoved = ifelse(Names %in% NamesRemoved, i-1, RoundRemoved))

    #Only if the target is edges
    OverloadedEdges <- ListofLists[[i]][[FinalGraph[i]]] %>%
      igraph::get.graph.attribute() %>%
      .$EdgesOverloaded

    RemovedDF <- RemovedDF %>%
      #Chooses entry by logically excluding options
      dplyr::mutate(RemovalType= dplyr::case_when(
        type == "Edge" & Names %in% OverloadedEdges ~ "Overloaded",
        Names %in% TargetNodeName ~ "Targeted",
        TRUE ~ RemovalType
      ))


  }

  return(RemovedDF)

}
