#' Find which edges are over the limit
#'
#'This function checks to see which lines are over limit and removes them and re-calculates power flow in the network
#'
#'The function uses a node attribute "bus_order" to identify the slack bus. This may be changed in later versions
#'
#' @param g An igraph object that represents a power-grid
#' @param edge_name the variable that holds the edge names, a character string.
#' @param node_name the variable that holds the names of the nodes, to identify the slack ref. a character string
#' @param net_generation the node_name that the net generation data for each node is held in
#' @export
calc_over_limit <- function(g, edge_name = "edge_name", node_name = "name", net_generation = "net_generation"){
  #This function calculates which edges are over the limit for the current network configuration and demand
  #production profile.
  #g: a graph with multiple edge and vertex attributes. The graph is assumed to be balenced in demand and production.
  #finds the slack reference in each component
  SlackRefCasc <-  slack_ref_func(g, node_name, generation = net_generation)
  #message(paste("Total network components ", nrow(SlackRefCasc))) #Not sure how useful this is anymore
  #Sometimes the Slackref df has 0 rows throwing an errors. so this code prevents that
  if(nrow(SlackRefCasc)!=0){
  #Calculate power flow for each component of the network as seperate networks
  gList <- 1:nrow(SlackRefCasc) %>%
    map(~{
      #print(paste("PowerFlow for componant", .x))
      slack_ref <- SlackRefCasc %>% slice(.x)
      gsubset <- delete.vertices(g, components(g)$membership != .x)
      if(slack_ref$nodes > 1){
        gsubset <- power_flow(gsubset, slack_ref$node_name,  edge_name, node_name, net_generation)
      }
      gsubset
    })
  gOut <- gList %>%
   Reduce(union_2, .)
} else
{
  gOut <- g
}
  return(gOut)
}
