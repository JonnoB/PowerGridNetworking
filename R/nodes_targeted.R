#' Nodes Targeted
#' 
#' Gets the node_name of the deleted target node/edge from the named attribute in each attack list.
#' @param network_list a list of lists of networks. The output of Attack the grid
#' @export
#' @seealso \code{\link{attack_the_grid}}
#' @examples
#' nodes_targeted(network_list)


nodes_targeted <-function(network_list){
  #Gets the node_name of the deleted target node/edge from the named attribute in each attack list.
  #Networklist: a list of lists of networks. The output of Attack the grid
  Out <- network_list %>% 
    map(~ get.graph.attribute(.x[[1]], "Removed")) %>% 
    unlist(.) %>% 
    c("None", .) #adds on the first element where there was nothing deleted.
  return(Out)
}
