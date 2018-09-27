#' Find Deletion order
#'
#' Attack round that each edge/node was deleted is  extracted from the network List of lists produced by AttackTheGrid
#'
#' If not all edges/nodes were deleted during AttackTheGrid, these edges will be filled with NA
#'
#' @param ListofLists The output of AttackTheGrid, a list of lists containing igraphs.
#' @param type Whether Edge or node order will be found. The default is Edge, any other value will mean nodes are used.
#' @export
#' @seealso \code{\link[PowerGridNetworking]{AttackTheGrid}}

RoundRemoved <- function(ListofLists, type = "Edge"){

  if(type =="Edge"){

    Removetype <- function(ListHere){
      ListHere %>% edge_attr(., "Link")
    }

  } else {
    Removetype <- function(ListHere){
      ListHere %>% vertex_attr(., "name")
    }
  }

  RemovedDF <- data_frame(Edges = Removetype(ListofLists[[1]][[1]]), RoundRemoved = NA )

  FinalGraph <-ListofLists %>%
    map_dbl(~length(.x))

  for(i in 2:length(FinalGraph)){

    EdgesRemoved <- difference(ListofLists[[i-1]][[FinalGraph[i-1]]],
                               ListofLists[[i]][[FinalGraph[i]]])  %>%
      Removetype()

    RemovedDF <- RemovedDF %>%
      mutate(RoundRemoved = ifelse(Edges %in% EdgesRemoved, i-1, RoundRemoved))

  }

  return(RemovedDF)

}
