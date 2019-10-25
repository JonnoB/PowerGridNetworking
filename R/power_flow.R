#' Calculate DC power flow
#'
#' Calculates the power_flow from a graph that contains the following attributes,
#' named edges, edgeweights, a balanced power generation and demand column. Writes an attribute called Powerflow 
#' (existing attributeof the same node_name will be overwritten). 
#' The function outputs a a graph with the correct power flow values
#' @param g An igraph object representing the power grid
#' @param slack_ref A character strong. The node to remove from the calculations to prevent the matrix being singular
#' @param edge_name The variable that holds the edge names, a character string.
#' @param node_name The variable that holds the names of the nodes, to identify the slack ref. a character string
#' @param net_generation The node_name that the net generation data for each node is held in
#' @keywords power flow
#' @export
#' @examples
#' power_flow(g Slackref)
power_flow <- function(g, slack_ref, edge_name = "edge_name", node_name = "name", net_generation = "net_generation"){
  #Calculates the power_flow from a graph that contains the following attributes
  #named edges, edgeweights, a balanced power generation and demand column, Powerflow (will be overwritten)
  #g: an igraph object
  #slack_ref: the node to remove from the calculations to prevent the matrix being singular
  InjectionVector <- get.vertex.attribute(g, node_name = net_generation)[get.vertex.attribute(g, node_name = node_name)!=slack_ref]
  Power <- imp_ptdf(g, slack_ref, edge_name, node_name)$ptdf %*% InjectionVector
  g <- set_edge_attr(g, "power_flow", value = Power)
  return(g)
}
