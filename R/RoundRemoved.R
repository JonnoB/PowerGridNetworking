#' Find Edge Deletion order
#'
#' Attack round that each edge was deleted is  extracted from the network List of lists produced by AttackTheGrid
#' 
#' If not all edges were deleted during AttackTheGrid, these edges will be filled with NA
#'
#' @param ListofLists The output of AttackTheGrid, a list of lists containing igraphs.
#' @export
#' @seealso \code{\link[PowerGridNetworking]{AttackTheGrid}}

RoundRemoved <- function(ListofLists){

  RemovedDF <- data_frame(Edges = edge_attr(ListofLists[[1]][[1]], "Link"), RoundRemoved = NA )
  
  FinalGraph <-ListofLists %>%
    map_dbl(~length(.x))
  
  for(i in 2:length(FinalGraph)){
    
    EdgesRemoved <- difference(ListofLists[[i-1]][[FinalGraph[i-1]]], 
                               ListofLists[[i]][[FinalGraph[i]]])  %>% 
      edge_attr(., "Link")
    
    RemovedDF <- RemovedDF %>%
      mutate(RoundRemoved = ifelse(Edges %in% EdgesRemoved, i-1, RoundRemoved))
    
  }

  return(RemovedDF)
  
}